with Ada.Containers;             use Ada.Containers;
with AVTAS.LMCP.Types;           use AVTAS.LMCP.Types;
with UxAS.Comms.LMCP_Net_Client; use UxAS.Comms.LMCP_Net_Client;
with LMCP_Messages;              use LMCP_Messages;
with Ada.Text_IO;                use Ada.Text_IO;

package body Waypoint_Plan_Manager with SPARK_Mode is

   use all type Pos64_WP_Maps.Formal_Model.M.Map;
   use all type Pos64_Nat64_Maps.Formal_Model.M.Map;
   use Pos64_Vectors.Formal_Model.M;

   --  function Has_Same_Keys
   --    (M : Pos64_WP_Map;
   --     N : Pos64_Nat64_Map) return Boolean
   --  is
   --    ((for all I of Model (M) => Has_Key (Model (N), I)) and then
   --       (for all I of Model (N) => Has_Key (Model (M), I)));

   --  function Waypoints_Are_Subset
   --    (Id_To_Waypoint : Pos64_WP_Map;
   --     WaypointList : WP_Seq) return Boolean
   --  is
   --    (for all Id of Model (Id_To_Waypoint) =>
   --          Contains (WaypointList, WP_Sequences.First, Last (WaypointList),
   --                    Element (Model (Id_To_Waypoint), Id)));

   --  function Elements_Are_Unique
   --    (V : Pos64_Vector) return Boolean
   --  is
   --    (for all I in Pos_Vec_M.First .. Last (Model (V)) =>
   --       (for all J in Pos_Vec_M.First .. Last (Model (V)) =>
   --            (if I /= J then
   --                  Element (Model (V), I) /= Element (Model (V), J))));

   --  function Id_Keys_Match_Waypoint_Ids
   --    (Id_To_Next_Id : Pos64_Nat64_Map;
   --     Id_To_Waypoint : Pos64_WP_Map) return Boolean
   --  is
   --    (for all Id of Model (Id_To_Next_Id) =>
   --          Contains (Model (Id_To_Waypoint), Id) and then
   --          Element (Model (Id_To_Next_Id), Id) =
   --          Element (Model (Id_To_Waypoint), Id).NextWaypoint);

   --  function Elements_Are_Successors
   --    (Id_To_Next_Id : Pos64_Nat64_Map;
   --     Path : Pos64_Vector) return Boolean
   --  is
   --    (for all I in Pos_Vec_M.First .. Last (Model (Path)) - 1 =>
   --        Successor (Model (Id_To_Next_Id), Element (Model (Path), I)) =
   --          Element (Model (Path), I + 1));

   --  function FirstWaypoint_Is_First_Or_Second_Element
   --    (FirstWaypoint : Pos64;
   --     Path : Pos64_Vector) return Boolean
   --  is
   --    (Element (Model (Path), 1) = FirstWaypoint or else
   --       (Length (Path) > 1 and then
   --        Element (Model (Path), 2) = FirstWaypoint));

   --  function Last_Element_Forms_Cycle
   --    (Id_To_Next_Id : Pos64_Nat64_Map;
   --     Path : Pos64_Vector) return Boolean
   --  is
   --    (Successor (Id_To_Next_Id, Last_Element (Path)) /= 0 and then
   --     Contains (Path, Successor (Id_To_Next_Id, Last_Element (Path))) and then
   --     Successor (Id_To_Next_Id, Last_Element (Path)) /= Last_Element (Path));

   --  function Cycle_Index_Is_Valid
   --    (Cycle_Index : Vector_Index;
   --     Path : Pos64_Vector;  -- Formal vector
   --     Id_To_Next_Id : Pos64_Nat64_Map) -- Formal hashed map
   --  return Boolean
   --  is
   --    (Cycle_Index in 1 .. Last_Index (Path) - 1 and then
   --     Element (Path, Cycle_Index) =
   --         Successor (Id_To_Next_Id, Last_Element (Path)));

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
       Waypoints_Are_Subset (Id_To_Waypoint, WaypointList) and then
       Has_Same_Keys (Id_To_Waypoint, Id_To_Next_Id) and then
       Id_Keys_Match_Waypoint_Ids (Id_To_Next_Id, Id_To_Waypoint);

   procedure Extract_MissionCommand_Maps
     (WaypointList : WP_Seq;
      Id_To_Waypoint : in out Pos64_WP_Map;
      Id_To_Next_Id : in out Pos64_Nat64_Map)
   is
      WP : Waypoint;
   begin

      Clear (Id_To_Next_Id);
      Clear (Id_To_Waypoint);

      for I in WP_Sequences.First .. Last (WaypointList) loop
         WP := Get (WaypointList, I);
         if WP.Number > 0 and then WP.NextWaypoint >= 0 then
            if not Contains (Id_To_Waypoint, Pos64 (WP.Number)) and then
              not Contains (Id_To_Next_Id, Pos64 (WP.Number))
            then
               Insert (Id_To_Waypoint, Pos64 (WP.Number), WP);
               Insert (Id_To_Next_Id, Pos64 (WP.Number), Nat64 (WP.NextWaypoint));
               pragma Assert
                 (Contains (WaypointList, WP_Sequences.First, Last (WaypointList),
                  Get (Model (Id_To_Waypoint), Pos64 (WP.Number))));
            end if;
         end if;

         pragma Loop_Invariant
           (Integer (Length (Id_To_Next_Id)) <= I - WP_Sequences.First + 1);
         pragma Loop_Invariant
           (Integer (Length (Id_To_Waypoint)) <= I - WP_Sequences.First + 1);
         pragma Loop_Invariant
           (Waypoints_Are_Subset (Id_To_Waypoint, WaypointList));
         pragma Loop_Invariant
           (Has_Same_Keys (Id_To_Waypoint, Id_To_Next_Id));
         pragma Loop_Invariant
           (Id_Keys_Match_Waypoint_Ids (Id_To_Next_Id, Id_To_Waypoint));
      end loop;

   end Extract_MissionCommand_Maps;

   --------------------
   -- Construct_Path --
   --------------------

   procedure Construct_Path
     (First_Id : Pos64;
      Id_To_Next_Id : Pos64_Nat64_Map;
      Path : in out Pos64_Vector;
      Next_Segment_Index : out Vector_Index;
      Cycle_Index : out Vector_Index)
     with
       Pre =>
         Is_Empty (Path) and then
         not Is_Empty (Id_To_Next_Id) and then
         Contains (Model (Id_To_Next_Id), First_Id),
       Post =>
         Next_Segment_Index = 1 and then
         not Is_Empty (Path) and then
         Next_Segment_Index <= Last_Index (Path) and then
         FirstWaypoint_Is_First_Or_Second_Element (First_Id, Path) and then
         (for all Id of Model (Path) => Contains (Model (Id_To_Next_Id), Id)) and then
         Elements_Are_Unique (Path) and then
         Elements_Are_Successors (Id_To_Next_Id, Path) and then
         (if Last_Element_Forms_Cycle (Id_To_Next_Id, Path) then
            Cycle_Index_Is_Valid (Cycle_Index, Path, Id_To_Next_Id)
          else
            Cycle_Index = 0);

   procedure Construct_Path
     (First_Id : Pos64;
      Id_To_Next_Id : Pos64_Nat64_Map;
      Path : in out Pos64_Vector;
      Next_Segment_Index : out Vector_Index;
      Cycle_Index : out Vector_Index)
   is
      use Pos64_Vectors.Formal_Model.M;
      use all type Pos64_Vectors.Formal_Model.M.Sequence;
   begin

      Next_Segment_Index := 1;
      Cycle_Index := 0;

      -- Append predecessor to First_Id to the list if found
      for Id of Id_To_Next_Id loop
         pragma Loop_Invariant (Is_Empty (Path));
         if not (Id = First_Id) and then
           Successor (Id_To_Next_Id, Id) = First_Id
         then
            Append (Path, Id);
            exit;
         end if;
      end loop;

      -- Append First_Id to the list
      Append (Path, First_Id);

      -- Append successors to the list, looking for cycle as we go
      while Length (Path) < Length (Id_To_Next_Id) loop
         if Last_Element_Forms_Cycle (Id_To_Next_Id, Path) then
            Cycle_Index :=
              Find_Index (Path,
                          Successor (Id_To_Next_Id, Last_Element (Path)));
            pragma Assert
              (Cycle_Index_Is_Valid (Cycle_Index, Path, Id_To_Next_Id));
            return;
         elsif Successor (Id_To_Next_Id, Last_Element (Path)) = 0 or else
           not Contains (Id_To_Next_Id, Pos64 (Successor (Id_To_Next_Id, Last_Element (Path)))) or else
           Contains (Path, Successor (Id_To_Next_Id, Last_Element (Path)))
         then
            return;
         else
            Append (Path, Successor (Id_To_Next_Id, Last_Element (Path)));
         end if;

         pragma Loop_Invariant (not Is_Empty (Path));
         pragma Loop_Invariant (Length (Model (Path)) >= 2);
         pragma Loop_Invariant
           (FirstWaypoint_Is_First_Or_Second_Element (First_Id, Path));
         pragma Loop_Invariant
           (for all Id of Model (Path) => Contains (Id_To_Next_Id, Id));
         pragma Loop_Invariant
           (Elements_Are_Successors (Id_To_Next_Id, Path));
         pragma Loop_Invariant
           (Elements_Are_Unique (Path));
      end loop;

      if Last_Element_Forms_Cycle (Id_To_Next_Id, Path) then
         Cycle_Index :=
           Find_Index (Path, Successor (Id_To_Next_Id, Last_Element (Path)));
      end if;
   end Construct_Path;

   ---------------------------
   -- Handle_MissionCommand --
   ---------------------------

   procedure Handle_MissionCommand
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
   is
      First_Id : constant Pos64 := MC.FirstWaypoint;
      Id_List : Pos64_Vector;
   begin

      -- Initialize relevant values of the state
      Clear (State.Id_To_Waypoint);
      State.MC := MC;
      State.New_Command := True;
      Clear (State.Path);
      State.Next_Segment_Index := 0;
      State.Cycle_Index := 0;
      Extract_MissionCommand_Maps
        (State.MC.WaypointList, State.Id_To_Waypoint, State.Id_To_Next_Id);

      -- Check whether the first waypoint ID can be found in the map of valid
      -- waypoint IDs. If not, return.
      if not Contains (State.Id_To_Next_Id, First_Id) then
         State.Next_First_Id := 0;
         return;
      end if;

      -- Otherwise, construct the path from which segments will be served
      Construct_Path (First_Id,
                      State.Id_To_Next_Id,
                      State.Path,
                      State.Next_Segment_Index,
                      State.Cycle_Index);

      State.Next_First_Id := First_Id;

   end Handle_MissionCommand;

   ------------------------
   -- Initialize_Segment --
   ------------------------

   procedure Lemma_Mod_Incr (A : Natural; B : Positive) with
     Ghost,
     Pre => A < Integer'Last,
     Post =>
       (if A mod B = B - 1 then (A + 1) mod B = 0
          else (A + 1) mod B = A mod B + 1);

   procedure Lemma_Mod_Incr (A : Natural; B : Positive) is null;

   procedure Initialize_Segment
     (Path : Pos64_Vector;
      Cycle_Index : Ext_Vector_Index;
      Path_Index : Vector_Index;
      Desired_Segment_Length : Positive;
      Overlap : Positive;
      Segment : in out Pos64_Vector;
      New_Path_Index : out Ext_Vector_Index;
      New_First_Id : out Nat64)
     with
       Pre =>
         Is_Empty (Segment) and then
         Length (Path) > 0 and then
         Last_Index (Path) <= Positive (Max) and then
         Path_Index in 1 .. Last_Index (Path) and then
         Cycle_Index in 0 .. Last_Index (Path) - 1 and then
         Overlap >= 2 and then Overlap < Positive (Max) and then
         Desired_Segment_Length > Overlap and then
         Desired_Segment_Length <= Positive (Max),
       Post =>
         Element (Model (Segment), 1) = Element (Model (Path), Path_Index) and then
         (for all Id of Model (Segment) =>
            Contains (Model (Path), 1, Last (Model (Path)), Id)) and then
         (if Cycle_Index > 0 then
            Positive (Length (Segment)) = Desired_Segment_Length and then
            Is_Subsegment (Path, Cycle_Index, Path_Index, Segment) and then
            New_Path_Index in 1 .. Last (Model (Path)) and then
            Element (Model (Segment), Last (Model (Segment)) - Overlap + 1) =
              Element (Model (Path), New_Path_Index) and then
            (if New_Path_Index < Last_Index (Path) then
                 New_First_Id = Element (Model (Path), New_Path_Index + 1)
             else
                 New_First_Id = Element (Model (Path), Cycle_Index))
          else
            (for all I in 1 .. Integer (Length (Segment)) =>
               Element (Model (Segment), I) = Element (Model (Path), Path_Index + I - 1)) and then
            (if Positive (Length (Path)) - Path_Index + 1 >= Desired_Segment_Length
             then
               Positive (Length (Segment)) = Desired_Segment_Length and then
               (if Last_Index (Path) = Last_Index (Segment) then
                  New_Path_Index = 0 and then New_First_Id = 0
                else
                  New_Path_Index = Path_Index + Desired_Segment_Length - Overlap and then
                    (Element (Model (Segment), Last (Model (Segment)) - Overlap + 1) =
                         Element (Model (Path), New_Path_Index)) and then
                  New_First_Id = Element (Model (Path), New_Path_Index + 1))
             else Positive (Length (Segment)) = Positive (Length (Path)) - Path_Index + 1 and then
               New_Path_Index = 0 and then New_First_Id = 0));

   procedure Initialize_Segment
     (Path : Pos64_Vector;
      Cycle_Index : Ext_Vector_Index;
      Path_Index : Vector_Index;
      Desired_Segment_Length : Positive;
      Overlap : Positive;
      Segment : in out Pos64_Vector;
      New_Path_Index : out Ext_Vector_Index;
      New_First_Id : out Nat64)
   is
      Len : Positive;
      PI : Positive := Path_Index;
      Seg_Overlap_Ind : Positive;
      use Pos64_Vectors.Formal_Model;
      use Pos64_Vectors.Formal_Model.M;
   begin

      New_Path_Index := 0;

      if Cycle_Index > 0 then
         Len := Desired_Segment_Length;
         Seg_Overlap_Ind := Len - Overlap + 1;
         for SI in 1 .. Len loop
            pragma Loop_Invariant
              (PI =
                 (if Path_Index + SI - 1 <= Last_Index (Path)
                  then Path_Index + SI - 1
                  else (Path_Index + SI - 1 - Last_Index (Path) - 1) mod
                    (Last_Index (Path) - Cycle_Index + 1) + Cycle_Index));
            pragma Loop_Invariant
              (if SI > Seg_Overlap_Ind then
                  New_Path_Index in 1 .. Last (Model (Path)) and then
                  (Element (Model (Segment), Seg_Overlap_Ind) =
                      Element (Model (Path), New_Path_Index)));
            pragma Loop_Invariant
              (Is_Subsegment (Path, Cycle_Index,
                              Path_Index, Segment));
            pragma Loop_Invariant
              (for all Id of Model (Segment) =>
                   Contains (Model (Path), 1, Last (Model (Path)), Id));
            pragma Loop_Invariant (Last_Index (Segment) = SI - 1);

            Append (Segment, Element (Path, PI));
            if Path_Index + SI - 1 > Last_Index (Path) then
               Lemma_Mod_Incr (Path_Index + SI - 1 - Last_Index (Path) - 1,
                               Last_Index (Path) - Cycle_Index + 1);
            end if;
            if SI = Seg_Overlap_Ind then
               New_Path_Index := PI;
               pragma Assert
                 (Element (Model (Segment), Seg_Overlap_Ind) =
                      Element (Model (Path), New_Path_Index));
            end if;
            if PI = Last_Index (Path) then
               PI := Cycle_Index;
            else
               PI := PI + 1;
            end if;

         end loop;
         if New_Path_Index < Last_Index (Path) then
            New_First_Id := Element (Path, New_Path_Index + 1);
         else
            New_First_Id := Element (Path, Cycle_Index);
         end if;

      else
         Len := (if Integer (Length (Path)) - Path_Index + 1 >=
                   Desired_Segment_Length
                 then Desired_Segment_Length
                 else Integer (Length (Path)) - Path_Index + 1);
         for I in 1 .. Len loop
            Append (Segment, Element (Path, PI));
            PI := PI + 1;
            pragma Loop_Invariant (PI = Path_Index + I);
            pragma Loop_Invariant (Integer (Length (Segment)) = I);
            pragma Loop_Invariant (Element (Model (Segment), 1) =
                                     Element (Model (Path), Path_Index));
            pragma Loop_Invariant
              (for all Id of Model (Segment) =>
                   Contains (Model (Path), 1, Last (Model (Path)), Id));
            pragma Loop_Invariant
              (for all I in 1 .. Integer (Length (Segment)) =>
                   Element (Model (Segment), I) = Element (Model (Path), Path_Index + I - 1));
         end loop;

         if Len = Desired_Segment_Length and then Last_Index (Path) /= Last_Index (Segment) then
            New_Path_Index := Path_Index + Desired_Segment_Length - Overlap;
            New_First_Id := Element (Path, New_Path_Index + 1);
         else
            New_Path_Index := 0;
            New_First_Id := 0;
         end if;

      end if;

   end Initialize_Segment;

   ---------------------
   -- Produce_Segment --
   ---------------------

   procedure Produce_Segment
     (State : in out Waypoint_Plan_Manager_State;
      Config : Waypoint_Plan_Manager_Configuration_Data;
      Mailbox : in out Waypoint_Plan_Manager_Mailbox)
   is
      Len : constant Positive := Positive (Config.NumberWaypointsToServe);
      Overlap : constant Positive := Positive (Config.NumberWaypointsOverlap);
      New_Path_Index : Ext_Vector_Index;
      New_First_Id : Nat64;

      use Pos64_Vectors.Formal_Model;
      use Pos64_Vectors.Formal_Model.M;
      use all type Pos64_Vectors.Formal_Model.M.Sequence;
   begin

      Clear (State.Segment);

      Initialize_Segment (State.Path,
                          State.Cycle_Index,
                          State.Next_Segment_Index,
                          Positive (Config.NumberWaypointsToServe),
                          Overlap,
                          State.Segment,
                          New_Path_Index,
                          New_First_Id);

      declare
         MC_Out : MissionCommand := State.MC;
      --     WP_List : WP_Seq;
      --     Id : Pos64;
      --     WP : Waypoint;
      begin
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
         sendBroadcastMessage (Mailbox, MC_Out);
      end;

      State.New_Command := False;
      State.Next_Segment_Index := New_Path_Index;
      State.Next_First_Id := New_First_Id;

   end Produce_Segment;

   procedure Lemma_Map_Still_Contains_List_After_Append
     (M : Pos64_Nat64_Map;
      L_Old, L_New : Pos64_Vector;
      New_Item : Pos64)
   is
   begin
      null;
   end Lemma_Map_Still_Contains_List_After_Append;

   procedure Lemma_List_Still_Linked_After_Append
     (M : Pos64_Nat64_Map;
      L_Old, L_New : Pos64_Vector;
      New_Item : Pos64)
   is
   begin
      null;
   end Lemma_List_Still_Linked_After_Append;

   procedure Lemma_First_Element_Unchanged_After_Append
     (V_Old, V_New : Pos64_Vector;
      First_Item : Pos64;
      New_Item : Pos64)
   is
   begin
      null;
   end Lemma_First_Element_Unchanged_After_Append;

end Waypoint_Plan_Manager;
