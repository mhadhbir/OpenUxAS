with Ada.Containers;             use Ada.Containers;
with AVTAS.LMCP.Types;           use AVTAS.LMCP.Types;
with UxAS.Comms.LMCP_Net_Client; use UxAS.Comms.LMCP_Net_Client;
with LMCP_Messages;              use LMCP_Messages;
with Ada.Text_IO;                use Ada.Text_IO;

package body Waypoint_Plan_Manager with SPARK_Mode is

   -- Pos64_WP_Maps.Formal_Model.M.Map
   -- Pos64_Nat64_Maps.Formal_Model.M.Map

   --  use all type Pos64_WP_Map;
   use all type Pos64_WP_Maps.Formal_Model.M.Map;
   --  use Pos64_WP_Maps.Formal_Model;
   --  use all type Pos64_Nat64_Map;
   use all type Pos64_Nat64_Maps.Formal_Model.M.Map;
   --  use Pos64_Nat64_Maps.Formal_Model;
   --  use Pos64_Vectors.Formal_Model;

   --  function Same_Mappings
   --    (M : Pos64_WP_Maps.Formal_Model.M.Map;
   --     N : Pos_WP_Maps_M.Map)
   --  return Boolean is
   --    ((for all I of M => Pos_WP_Maps_M.Has_Key (N, I))
   --     and then (for all I of N => Contains (M, I))
   --     and then
   --       (for all I of N =>
   --            (for all E of Pos_WP_Maps_M.Get (N, I) =>
   --                    Contains (Element (M, I), E)))
   --     and then
   --       (for all I of N =>
   --            (for all E of Element (M, I) =>
   --                    Contains (Pos_WP_Maps_M.Get (N, I), E))));

   procedure Lemma_Map_Still_Contains_List_After_Append
     (M : Pos64_Nat64_Map;
      L_Old, L_New : Pos64_Vectors.Vector;
      New_Item : Pos64)
   is
   begin
      null;
   end Lemma_Map_Still_Contains_List_After_Append;

   procedure Lemma_List_Still_Linked_After_Append
     (M : Pos64_Nat64_Map;
      L_Old, L_New : Pos64_Vectors.Vector;
      New_Item : Pos64)
   is
   begin
      null;
   end Lemma_List_Still_Linked_After_Append;

   procedure Lemma_First_Element_Unchanged_After_Append
     (V_Old, V_New : Pos64_Vectors.Vector;
      First_Item : Pos64;
      New_Item : Pos64)
   is
   begin
      null;
   end Lemma_First_Element_Unchanged_After_Append;

   --  function Has_Same_Keys
   --    (M : Pos64_WP_Maps.Formal_Model.M.Map;
   --     N : Pos64_Nat64_Maps.Formal_Model.M.Map) return Boolean
   --  with
   --    Ghost,
   --    Global => null;
   --  --  Annotate => (GNATprove, Inline_For_Proof);

   function Has_Same_Keys
     (M : Pos64_WP_Maps.Formal_Model.M.Map;
      N : Pos64_Nat64_Maps.Formal_Model.M.Map) return Boolean
   is
     ((for all I of M => Has_Key (N, I)) and then
        (for all I of N => Has_Key (M, I)));

   function Waypoints_Are_Subset
     (WaypointList : WP_Seq;
      Id_To_Waypoint : Pos64_WP_Map) return Boolean
   is
     (for all Id of Model (Id_To_Waypoint) =>
           Contains (WaypointList, WP_Sequences.First, Last (WaypointList),
                     Get (Model (Id_To_Waypoint), Id)));

   function Elements_Are_Unique
     (V : Pos64_Vector) return Boolean
   is
     (for all I in Pos_Vec_M.First .. Pos_Vec_M.Last (Model (V)) =>
        (for all J in Pos_Vec_M.First .. Pos_Vec_M.Last (Model (V)) =>
             (if I /= J then
                     Element (Model (V), I) /= Element (Model (V), J))));

   function Id_Keys_Match_Waypoint_Ids
     (Id_To_Next_Id : Pos64_Nat64_Map;
      Id_To_Waypoint : Pos64_WP_Map) return Boolean
   is
     (for all Id of Model (Id_To_Next_Id) =>
           Element (Model (Id_To_Next_Id), Id) =
           Element (Model (Id_To_Waypoint), Id).NextWaypoint);

   ---------------------------------
   -- Extract_MissionCommand_Maps --
   ---------------------------------

   procedure Extract_MissionCommand_Maps
     (WaypointList : WP_Seq;
      Id_To_Waypoint : in out Pos64_WP_Map;
      Id_To_Next_Id : in out Pos64_Nat64_Map)
   with
     Pre => Length (WaypointList) <= Max,
     Post =>
       Waypoints_Are_Subset (WaypointList, Id_To_Waypoint) and then
       Has_Same_Keys (Model (Id_To_Waypoint), Model (Id_To_Next_Id)) and then
       Id_Keys_Match_Waypoint_Ids (Id_To_Next_Id, Id_To_Waypoint);

   procedure Extract_MissionCommand_Maps
     (WaypointList : WP_Seq;
      Id_To_Waypoint : in out Pos64_WP_Map;
      Id_To_Next_Id : in out Pos64_Nat64_Map)
   is
      WP : Waypoint;
      Id_To_Next_Id_Tmp : Pos64_Nat64_Map with Ghost;
   begin

      Clear (Id_To_Next_Id);
      Clear (Id_To_Waypoint);

      for I in WP_Sequences.First .. Last (WaypointList) loop
         WP := Get (WaypointList, I);
         if WP.Number > 0 and then WP.NextWaypoint >= 0 then
            Id_To_Next_Id_Tmp := Id_To_Next_Id;
            if not Contains (Id_To_Waypoint, Pos64 (WP.Number)) and then
              not Contains (Id_To_Next_Id, Pos64 (WP.Number))
            then
               Insert (Id_To_Waypoint, Pos64 (WP.Number), WP);
               pragma Assert (Id_To_Next_Id = Id_To_Next_Id_Tmp);
               Insert (Id_To_Next_Id, Pos64 (WP.Number), Nat64 (WP.NextWaypoint));
               pragma Assert
                 (Contains (WaypointList, WP_Sequences.First, Last (WaypointList),
                  Get (Model (Id_To_Waypoint), Pos64 (WP.Number))));
            end if;
         end if;

         pragma Loop_Invariant (Integer (Length (Id_To_Next_Id)) <= I - WP_Sequences.First + 1);
         pragma Loop_Invariant (Integer (Length (Id_To_Waypoint)) <= I - WP_Sequences.First + 1);
         pragma Loop_Invariant
           (Waypoints_Are_Subset (WaypointList, Id_To_Waypoint));
         pragma Loop_Invariant
           (Has_Same_Keys (Model (Id_To_Waypoint), Model (Id_To_Next_Id)));
         pragma Loop_Invariant
           (Id_Keys_Match_Waypoint_Ids (Id_To_Next_Id, Id_To_Waypoint));
      end loop;

   end Extract_MissionCommand_Maps;

   ---------------------------
   -- Handle_MissionCommand --
   ---------------------------

   procedure Handle_MissionCommand
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
   is
      First_Id : constant Pos64 := MC.FirstWaypoint;
      Id_List : Pos64_Vector;
      Id_List_Tmp : Pos64_Vector with Ghost;
      function Successor (M : Pos64_Nat64_Map; K : Pos64) return Nat64
                          renames Element;

      use Pos64_Vectors.Formal_Model.M;
      use all type Pos64_Vectors.Formal_Model.M.Sequence;
   begin

      Clear (State.Id_To_Waypoint);
      State.MC := MC;
      State.New_Command := True;
      Clear (State.Path);
      State.Next_Segment_Index := 0;
      State.Cycle_Index := 0;

      Extract_MissionCommand_Maps
        (State.MC.WaypointList, State.Id_To_Waypoint, State.Id_To_Next_Id);

      -- Check whether First_Id can be found. If not, return.
      if not Contains (State.Id_To_Next_Id, First_Id) then
         -- pragma Assert (if State.Cycle_Index = 0 then Is_Empty (State.Path));
         return;
      end if;

      -- Check whether there is a predecessor to First_Id that isn't just
      -- First_Id itself. If so, it becomes the first element of the list.
      for Id of State.Id_To_Next_Id loop
         if not (Id = First_Id) and then
           Successor (State.Id_To_Next_Id, Id) = First_Id
         then
            Append (Id_List, Id);
            State.Next_Segment_Index := 1;
            exit;
         end if;
      end loop;

      -- Append First_Id to the list
      Append (Id_List, First_Id);

      -- Assert properties needed for container min and max size
      pragma Assert (not Is_Empty (Id_List));
      pragma Assert (Length (Id_List) <= 2);

      pragma Assert (if Contains (State.Id_To_Next_Id, MC.FirstWaypoint) then
                       (Element (Model (Id_List), 1) = First_Id or else
                        Element (Model (Id_List), 2) = First_Id));

      pragma Assert (for all I in First_Index (Id_List) .. Last_Index (Id_List) - 1 =>
                       Successor (State.Id_To_Next_Id, Element (Model (Id_List), I)) =
                       Element (Model (Id_List), I + 1));

      pragma Assert
        (for all I of Model (Id_List) => Contains (State.Id_To_Next_Id, I));

      pragma Assert
        (Elements_Are_Unique (Id_List));

      pragma Assert (Waypoints_Are_Subset (MC.WaypointList, State.Id_To_Waypoint));

      while Length (Id_List) < Length (State.Id_To_Next_Id) loop

         if Successor (State.Id_To_Next_Id, Last_Element (Id_List)) = 0
           or else Successor (State.Id_To_Next_Id, Last_Element (Id_List)) = Last_Element (Id_List)
         then
            -- Candidate successor is 0 or points to itself.
            -- There is no cycle. Set the path and return.
            State.Path := Id_List;
            return;
         elsif not Contains (State.Id_To_Next_Id,
                             Pos64 (Successor (State.Id_To_Next_Id, Last_Element (Id_List))))
         then
            -- Candidate successor can't be found.
            -- There is no cycle. Set the path and return.
            State.Path := Id_List;
            return;
         elsif Contains (Id_List, Successor (State.Id_To_Next_Id, Last_Element (Id_List)))
         then
            -- There is a cycle.
            -- Set the cycle id and path and return.
            State.Cycle_Index := Find_Index (Id_List, Successor (State.Id_To_Next_Id, Last_Element (Id_List)));
            pragma Assert (State.Cycle_Index > 0);
            -- State.Cycle_Id := Successor (State.Id_To_Next_Id, Last_Element (Id_List));
            State.Path := Id_List;
            return;
         elsif Contains (State.Id_To_Next_Id, Pos64 (Successor (State.Id_To_Next_Id, Last_Element (Id_List))))
         then
            -- Found a successor that's not a cycle.
            -- Append it to the list and continue.
            declare
               Current_Id : constant Pos64 := Last_Element (Id_List);
               Succ : constant Pos64 :=
                 Successor (State.Id_To_Next_Id, Last_Element (Id_List)) with Ghost;
            begin

               Id_List_Tmp := Id_List;

               pragma Assert
                 (for all I of Model (Id_List) => Contains (State.Id_To_Next_Id, I));

               Append (Id_List, Successor (State.Id_To_Next_Id, Last_Element (Id_List)));

            end;
         else
            -- Should be unreachable.
            -- Making all previous cases explicit seems to aid proof.
            raise Program_Error;
         end if;

         pragma Loop_Invariant (not Is_Empty (Id_List));
         pragma Loop_Invariant (Length (Model (Id_List)) >= 2);

         pragma Loop_Invariant (State.Id_To_Next_Id = State'Loop_Entry.Id_To_Next_Id);
         pragma Loop_Invariant (State.Id_To_Waypoint = State.Id_To_Waypoint'Loop_Entry);

         pragma Loop_Invariant (Element (Model (Id_List), 1) = First_Id or else
                                Element (Model (Id_List), 2) = First_Id);

         pragma Loop_Invariant
           (Waypoints_Are_Subset (MC.WaypointList, State.Id_To_Waypoint));

         pragma Loop_Invariant
           (for all Id of Model (Id_List) => Contains (State.Id_To_Next_Id, Id));

         pragma Loop_Invariant
           (for all I in First_Index (Id_List) .. Last_Index (Id_List) - 1 =>
              Successor (State.Id_To_Next_Id, Element (Id_List, I)) =
              Element (Id_List, I + 1));

         pragma Loop_Invariant
           (Elements_Are_Unique (Id_List));

         pragma Loop_Invariant (State.Cycle_Index = 0);
      end loop;

      State.Path := Id_List;

   end Handle_MissionCommand;

   ---------------------
   -- Produce_Segment --
   ---------------------

   procedure Initialize_Segment
     (Path : Pos64_Vector;
      Desired_Segment_Length : Positive;
      Cycle_Index : Natural;
      Path_Index : in out Positive;
      Segment : in out Pos64_Vector)
     with
       Pre =>
         Is_Empty (Segment) and then
         Length (Path) > 0 and then
         Iter_Has_Element (Path, Path_Index) and then
         (if Cycle_Index > 0 then Iter_Has_Element (Path, Cycle_Index)) and then
         Desired_Segment_Length <= Positive (Max),
       Post =>
         Element (Segment, 1) = Element (Path, Path_Index'Old) and then
         (for all Id of Segment => Contains (Path, Id)) and then
         (if Cycle_Index > 0
          then Positive (Length (Segment)) = Desired_Segment_Length --) and then
         --(if Cycle_Index = 0
          --then
          else
            (if Positive (Length (Path)) - Path_Index'Old + 1 >= Desired_Segment_Length
             then Positive (Length (Segment)) = Desired_Segment_Length
             else Positive (Length (Segment)) = Positive (Length (Path)) - Path_Index'Old + 1));

   procedure Initialize_Segment
     (Path : Pos64_Vector;
      Desired_Segment_Length : Positive;
      Cycle_Index : Natural;
      Path_Index : in out Positive;
      Segment : in out Pos64_Vector)
   is
      Len : Positive;
      Initial_Path_Index : constant Positive := Path_Index with Ghost;
      Segment_Tmp : Pos64_Vector with Ghost;

      use Pos64_Vectors.Formal_Model;
      use Pos64_Vectors.Formal_Model.M;
   begin

      if Cycle_Index > 0 then
         Len := Desired_Segment_Length;
         for I in 1 .. Len loop
            if not Iter_Has_Element (Path, Path_Index) then
               Path_Index := Cycle_Index;
            end if;
            Append (Segment, Element (Path, Path_Index));
            Path_Index := Path_Index + 1;

            -- Path_Index = Initial_Path_Index + I - 1
            --  pragma Loop_Invariant
            --    (if Initial_Path_Index + I - 1 < Last_Index (Path)
            --     then
            --        Path_Index = Initial_Path_Index + I - 1
            --     else
            --        Path_Index =
            --       ((Initial_Path_Index + I - 1 - Integer (Length (Path))) mod (Integer (Length (Path)) - Cycle_Index + 1)) + Cycle_Index - 1);
            pragma Loop_Invariant
              (Element (Model (Segment), 1) =
                 Element (Model (Path), Initial_Path_Index));
            pragma Loop_Invariant (Integer (Length (Segment)) = I);
            pragma Loop_Invariant
              (for all Id of Segment => Contains (Path, Id));
         end loop;
      else
         Len := (if Integer (Length (Path)) - Path_Index + 1 >=
                   Desired_Segment_Length
                 then Desired_Segment_Length
                 else Integer (Length (Path)) - Path_Index + 1);
         Append (Segment, Element (Path, Path_Index));
         Path_Index := Path_Index + 1;
         for I in 2 .. Len loop
            Append (Segment, Element (Path, Path_Index));
            Path_Index := Path_Index + 1;
            pragma Loop_Invariant (Path_Index = Initial_Path_Index + I);
            pragma Loop_Invariant (Integer (Length (Segment)) = I);
            pragma Loop_Invariant (Element (Segment, 1) = Element (Path, Initial_Path_Index));
            pragma Loop_Invariant
              (for all J in 2 .. I =>
                 (Element (Segment, J - 1) =
                      Element (Path, Initial_Path_Index + J - 2) and then
                  Element (Segment, J) =
                      Element (Path, Initial_Path_Index + J - 1)));
            pragma Loop_Invariant
              (for all Id of Segment => Contains (Path, Id));
         end loop;
      end if;

   end Initialize_Segment;

   procedure Produce_Segment
     (State : in out Waypoint_Plan_Manager_State;
      Config : Waypoint_Plan_Manager_Configuration_Data;
      Mailbox : in out Waypoint_Plan_Manager_Mailbox)
   is
      Len : constant Positive := Positive (Config.NumberWaypointsToServe);
      Overlap : constant Positive := Positive (Config.NumberWaypointsOverlap);

      use Pos64_Vectors.Formal_Model;
      use Pos64_Vectors.Formal_Model.M;
      use all type Pos64_Vectors.Formal_Model.M.Sequence;
   begin

      Clear (State.Segment);

      Initialize_Segment (State.Path,
                          Integer (Config.NumberWaypointsToServe),
                          State.Cycle_Index,
                          State.Next_Segment_Index,
                          State.Segment);

      if State.Cycle_Index > 0 then
         declare
            Next_Segment_Id : Pos64;
         begin
            Next_Segment_Id :=
              Element (State.Segment,
                       Integer (Length (State.Segment)) - Overlap + 1);
            State.Next_Segment_Index :=
              Find_Index (State.Path, Next_Segment_Id);
         end;
         pragma Assert (Iter_Has_Element (State.Path, State.Next_Segment_Index));
      else
         if Positive (Length (State.Segment)) = Len and then
           Element (State.Segment, Last_Index (State.Segment)) /=
             Element (State.Path, Last_Index (State.Path))
         then
            declare
               Next_Segment_Id : Pos64;
            begin
               Next_Segment_Id :=
                 Element (State.Segment,
                       Integer (Length (State.Segment)) - Overlap + 1);
               State.Next_Segment_Index :=
                 Find_Index (State.Path, Next_Segment_Id);
            end;
            pragma Assert (Iter_Has_Element (State.Path, State.Next_Segment_Index));
         else
            State.Next_Segment_Index := 0;
         end if;
      end if;

      State.New_Command := False;

      --  declare
      --     MC_Out : MissionCommand := State.MC;
      --     WP_List : WP_Seq;
      --     Id : Pos64;
      --     WP : Waypoint;
      --  begin
      --     -- MC_Out.FirstWaypoint := First_Id;
      --     MC_Out.FirstWaypoint :=
      --       (if Length (State.Segment) > 1
      --        then Element (State.Segment, 2)
      --        else Element (State.Segment, 1));
      --     for I in First_Index (State.Segment) .. Last_Index (State.Segment) loop
      --        Id := Element (State.Segment, I);
      --        --if Contains (State.Id_To_Waypoint, Id) then
      --           WP := Element (State.Id_To_Waypoint, Id);
      --           if I = Last_Index (State.Segment) then
      --              WP.NextWaypoint := WP.Number;
      --              -- TODO: Extend SPARK messages to handle
      --              -- VehicleAction -> NavigationAction -> LoiterAction
      --              -- VehicleAction -> PayloadAction -> GimbalAngleAction
      --           end if;
      --           -- WP.TurnType := Config.TurnType;
      --           WP_List := Add (WP_List, WP);
      --        --end if;
      --        pragma Loop_Invariant
      --          (Integer (Length (WP_List)) <= I - First_Index (State.Segment) + 1);
      --     end loop;
      --     MC_Out.WaypointList := WP_List;
      --     sendBroadcastMessage (Mailbox, MC_Out);
      --  end;

   end Produce_Segment;

end Waypoint_Plan_Manager;
