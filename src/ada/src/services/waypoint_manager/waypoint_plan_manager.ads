with SPARK.Containers.Formal.Hashed_Maps;
with SPARK.Containers.Formal.Vectors;
with SPARK.Containers.Functional.Maps;

with Ada.Containers;                             use all type Ada.Containers.Count_Type;
with Waypoint_Plan_Manager_Communication;        use Waypoint_Plan_Manager_Communication;
with Common;                                     use Common;
with LMCP_Messages;                              use LMCP_Messages;

package Waypoint_Plan_Manager with SPARK_Mode is

   Max : constant Ada.Containers.Count_Type := 2000;

   subtype Pos64 is Int64 range 1 .. Int64'Last;

   subtype Nat64 is Int64 range 0 .. Int64'Last;

   function Pos64_Hash (X : Pos64) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (X));

   package Pos64_WP_Maps is new SPARK.Containers.Formal.Hashed_Maps (Pos64, Waypoint, Pos64_Hash);
   use Pos64_WP_Maps;
   package Pos_WP_Maps_P renames Pos64_WP_Maps.Formal_Model.P;
   package Pos_WP_Maps_K renames Pos64_WP_Maps.Formal_Model.K;
   package Pos_WP_Maps_M is new SPARK.Containers.Functional.Maps (Pos64, Waypoint);
   subtype Pos64_WP_Map is Pos64_WP_Maps.Map (Max, Pos64_WP_Maps.Default_Modulus (Max))
     with Predicate =>
              (for all Id of Pos64_WP_Map =>
                 (Element (Pos64_WP_Map, Id).Number = Id and
                      Element (Pos64_WP_Map, Id).NextWaypoint >= 0));

   package Pos64_Nat64_Maps is new SPARK.Containers.Formal.Hashed_Maps (Pos64, Nat64, Pos64_Hash);
   use Pos64_Nat64_Maps;
   package Pos_Nat_Maps_P renames Pos64_Nat64_Maps.Formal_Model.P;
   package Pos_Nat_Maps_K renames Pos64_Nat64_Maps.Formal_Model.K;
   package Pos_Nat_Maps_M is new SPARK.Containers.Functional.Maps (Pos64, Nat64);
   subtype Pos64_Nat64_Map is Pos64_Nat64_Maps.Map (Max, Pos64_Nat64_Maps.Default_Modulus (Max));

   package Pos64_Vectors is new SPARK.Containers.Formal.Vectors (Positive, Pos64);
   subtype Pos64_Vector is Pos64_Vectors.Vector (Max + 1);
   package Pos_Vec_M renames Pos64_Vectors.Formal_Model.M;
   use Pos64_Vectors;

   use Pos64_WP_Maps.Formal_Model;
   use Pos64_Nat64_Maps.Formal_Model;
   use Pos64_Vectors.Formal_Model;

   --  function Same_Mappings
   --    (M : Pos64_WP_Maps.Formal_Model.M.Map;
   --     N : Pos_WP_Maps_M.Map)
   --     return Boolean
   --  with Ghost,
   --       Annotate => (GNATprove, Inline_For_Proof);
   --
   --  function Model (M : Pos64_WP_Map) return Pos_WP_Maps_M.Map with
   --    Post => Same_Mappings
   --      (Pos64_WP_Maps.Formal_Model.Model (M), Model'Result);

   procedure Lemma_Map_Still_Contains_List_After_Append
     (M : Pos64_Nat64_Map;
      L_Old, L_New : Pos64_Vectors.Vector;
      New_Item : Pos64)
     with Ghost,
       Pre =>
         Length (L_Old) < Capacity (L_Old) and then
         Length (L_New) = Length (L_Old) + 1 and then
         Model (L_Old) < Model (L_New) and then
         Element (L_New, Last_Index (L_Old) + 1) = New_Item and then
         Contains (M, New_Item) and then
         (for all Item of Model (L_Old) => Contains (M, Item)),
       Post =>
         (for all Item of Model (L_New) => Contains (M, Item));

   procedure Lemma_First_Element_Unchanged_After_Append
     (V_Old, V_New : Pos64_Vectors.Vector;
      First_Item : Pos64;
      New_Item : Pos64)
     with Ghost,
     Pre =>
       Length (V_Old) >= 1 and then
       Length (V_Old) < Capacity (V_Old) and then
       Length (V_New) = Length (V_Old) + 1 and then
       Model (V_Old) < Model (V_New) and then
       Element (Model (V_Old), 1) = First_Item and then
       Element (Model (V_New), Last_Index (V_Old) + 1) = New_Item,
     Post =>
       Element (Model (V_New), 1) = First_Item;

   procedure Lemma_List_Still_Linked_After_Append
     (M : Pos64_Nat64_Map;
      L_Old, L_New : Pos64_Vectors.Vector;
      New_Item : Pos64)
     with
       Ghost,
       Pre =>
         Length (L_Old) < Capacity (L_Old) and then
         not Is_Empty (L_Old) and then
         Length (L_New) = Length (L_Old) + 1 and then
         Model (L_Old) < Model (L_New) and then
         Element (L_New, Last_Index (L_Old) + 1) = New_Item and then
         (for all Item of Model (L_Old) => Contains (M, Item)) and then
         Element (M, Element (L_Old, Last_Index (L_Old))) = New_Item and then
         (for all I in First_Index (L_Old) .. Last_Index (L_Old) - 1 =>
            Element (M, Element (L_Old, I)) = Element (L_Old, I + 1)),
       Post =>
         (for all I in First_Index (L_New) .. Last_Index (L_New) - 1 =>
            Element (M, Element (L_New, I)) = Element (L_New, I + 1));

   type Waypoint_Plan_Manager_Configuration_Data is record
      -- Number of waypoints remaining before starting the next segment.
      -- Value must be in [2, Max - 1]
      NumberWaypointsOverlap : Common.UInt32 := 2;
      -- Max number of waypoints to serve for each segment.
      -- Value must be in [NumberWaypointsOverlap + 1, Max].
      -- Default to Max to serve them all.
      NumberWaypointsToServe : Common.UInt32 := Common.UInt32 (Max);
      -- Radius to use for loiters added by the waypoint manager
      LoiterRadiusDefault : Common.Real64 := 200.0;
      -- Turn type to use for loiters added by the waypoint manager
      TurnType : LMCP_Messages.TurnTypeEnum := TurnShort;
      -- Gimbal payload ID to use for loiters added by the waypoint manager
      GimbalPayloadId : Common.Int64 := -1;
      -- Vehicle ID of managed vehicle
      VehicleID : Common.Int64 := -1;
   end record;

   type Waypoint_Plan_Manager_State is record
      MC : MissionCommand;  -- Copy of most recent MissionCommand
      Id_To_Waypoint : Pos64_WP_Map;   -- Id -> copy of Waypoint message from MC
      Id_To_Next_Id : Pos64_Nat64_Map; -- Id -> successor waypoint Id in MC
      Path : Pos64_Vector;  -- Ordered list of waypoint Ids, built using info in -- Id_To_Next_Id, starting from MC.FirstWaypoint
      Cycle_Id : Nat64; -- If MC.WaypointList contains a cycle, this is the
                        -- value in Path to cycle back to after reaching the end
      Next_Segment_Id : Nat64 := 0; -- 1st Id of next segment
      Next_First_Id : Nat64 := 0; -- 2nd Id of next segment (MC.FirstWaypoint)
      Segment : Pos64_Vector; -- Next segment
      New_Command : Boolean; -- Whether the most recent MissionCommand has yet -- been used to produce a segment
      Headed_To_First_Id : Boolean := False; -- Whether vehicle has reached
                                             -- FirstWaypoint of next segment
      Next_Segment_Index_In_Path : Natural;
      Cycle_Index_In_Path : Natural;
   end record;

   procedure Handle_MissionCommand
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
     with
       Pre =>
         Length (MC.WaypointList) <= Max and then
         MC.FirstWaypoint > 0,
       Post =>
         State.MC = MC and then
         -- Every Waypoint stored by Id in Id_To_Waypoint came from MC.WaypointList
         --  (for all Id of Model (State.Id_To_Waypoint) =>
         --     Contains (MC.WaypointList, WP_Sequences.First, Last (State.MC.WaypointList),
         --               Element (Model (State.Id_To_Waypoint), Id))) and then
         -- Basic relationships: Id_To_Waypoint and Id_To_Next_Id should have
         -- the same keys, and each (Id, NextId) of Id_To_Next_Id should match
         -- the corresponding (Id, Waypoint.NextWaypoint) of Id_To_Waypoint
         (for all Id of Model (State.Id_To_Waypoint) =>
            Contains (Model (State.Id_To_Next_Id), Id)) and then
         (for all Id of Model (State.Id_To_Next_Id) =>
           Contains (Model (State.Id_To_Waypoint), Id)) and then
         (for all Id of Model (State.Id_To_Next_Id) =>
            Element (Model (State.Id_To_Next_Id), Id) =
              Element (Model (State.Id_To_Waypoint), Id).NextWaypoint) and then
         -- If MC.FirstWaypoint cannot be found in Id_To_Next_Id,
         -- then path, cycle, and "next" segment info should be 0/empty.
         -- Otherwise, MC.FirstWaypoint should be element 1 or 2 of Path.
         (if not Contains (Model (State.Id_To_Next_Id), MC.FirstWaypoint)
            then State.Next_Segment_Id = 0 and State.Next_First_Id = 0 and
                 State.Cycle_Id = 0 and Is_Empty (State.Path)) and then
         (if Contains (Model (State.Id_To_Next_Id), MC.FirstWaypoint)
            then (Element (Model (State.Path), 1) = MC.FirstWaypoint or else
                  Element (Model (State.Path), 2) = MC.FirstWaypoint)) and then
         -- Every Id in Path should come from Id_To_Next_Id, be unique, and its
         -- successor should be corresponding value stored in Id_To_Next_Id
         (for all Id of Model (State.Path) => Contains (State.Id_To_Next_Id, Id)) and then
         (for all I in Pos_Vec_M.First .. Pos_Vec_M.Last (Model (State.Path)) =>
            (for all J in Pos_Vec_M.First .. Pos_Vec_M.Last (Model (State.Path)) =>
                 (if I /= J then Element (Model (State.Path), I) /=
                      Element (Model (State.Path), J)))) and then
         (for all I in First_Index (State.Path) .. Last_Index (State.Path) - 1 =>
            Element (State.Id_To_Next_Id, Element (State.Path, I)) =
              Element (State.Path, I + 1)) and then
         -- If Cycle_Id is non-zero, it should be the successor to the last
         -- Id in Path according to Id_To_Next_Id, and it should be in elsewhere
         -- in Path. Otherwise, Path should be empty, or the successor to the
         -- last Id in Path should be 0, itself, or not found in Id_To_Next_Id.
         (if State.Cycle_Id > 0 then
            State.Cycle_Id =
              Element (Model (State.Id_To_Next_Id),
                       Pos_Vec_M.Get (Model (State.Path),
                                      Pos_Vec_M.Last (Model (State.Path))))
          and Pos_Vec_M.Contains (Model (State.Path),
                                  Pos_Vec_M.First,
                                  Pos_Vec_M.Last (Model (State.Path)),
                                  State.Cycle_Id)) and then
          (if State.Cycle_Id = 0 then
             (Is_Empty (State.Path) or else
              Element (Model (State.Id_To_Next_Id),
                       Pos_Vec_M.Get (Model (State.Path),
                                      Pos_Vec_M.Last (Model (State.Path)))) = 0 or else
              Element (Model (State.Id_To_Next_Id),
                       Pos_Vec_M.Get (Model (State.Path),
                                      Pos_Vec_M.Last (Model (State.Path)))) =
                       Pos_Vec_M.Get (Model (State.Path),
                                      Pos_Vec_M.Last (Model (State.Path))) or else
              not Contains (Model (State.Id_To_Next_Id),
                            Pos_Vec_M.Get (Model (State.Path),
                                           Pos_Vec_M.Last (Model (State.Path))))));

   procedure Produce_Segment
     (State : in out Waypoint_Plan_Manager_State;
      Config : Waypoint_Plan_Manager_Configuration_Data;
      Mailbox : in out Waypoint_Plan_Manager_Mailbox)
     with
       Pre =>
         Config.NumberWaypointsOverlap >= 2 and then
         Config.NumberWaypointsOverlap <= UInt32 (Max) - 1 and then
         Config.NumberWaypointsToServe > Config.NumberWaypointsOverlap and then
         Config.NumberWaypointsToServe <= UInt32 (Max) and then
         Length (State.Path) > 0 and then
         Iter_Has_Element (State.Path, State.Next_Segment_Index_In_Path) and then
         (if State.Cycle_Index_In_Path > 0 then
            Iter_Has_Element (State.Path, State.Cycle_Index_In_Path)) and then
         (for all Id of State.Path => Contains (State.Id_To_Waypoint, Id)),
         Post =>
           -- The line below should NOT prove but is proving
           Integer (Length (State.Segment)) = 4 and then
           -- The rest of this is as I think it should be
           State'Old.MC = State.MC and then
           State'Old.Id_To_Waypoint = State.Id_To_Waypoint and then
           State'Old.Id_To_Next_Id = State.Id_To_Next_Id and then
           State'Old.Path = State.Path and then
           State'Old.Cycle_Index_In_Path = State.Cycle_Index_In_Path and then
           State'Old.Headed_To_First_Id = State.Headed_To_First_Id and then
           State.New_Command = False and then
           Element (State.Segment, 1) = Element (State.Path, State'Old.Next_Segment_Index_In_Path) and then
           (for all Id of State.Segment => Contains (State.Path, Id)) and then
           (if State.Cycle_Index_In_Path > 0
              then
                (Integer (Length (State.Segment)) = Integer (Config.NumberWaypointsToServe) and then
                   (Element (State.Path, State.Next_Segment_Index_In_Path) =
                      Element (State.Segment,
                               Integer (Length (State.Segment)) -
                                 Integer (Config.NumberWaypointsOverlap) + 1)))
                  else
              (if Positive (Length (State.Path)) - State'Old.Next_Segment_Index_In_Path + 1 >=
                 Integer (Config.NumberWaypointsToServe)
                   then
                 (Positive (Length (State.Segment)) = Integer (Config.NumberWaypointsToServe) and then
                      Element (State.Path, State.Next_Segment_Index_In_Path) =
                    Element (State.Segment, Integer (Length (State.Segment)) - Integer (Config.NumberWaypointsOverlap) + 1))
                   else
                 (Positive (Length (State.Segment)) =
                      Positive (Length (State.Path)) - State'Old.Next_Segment_Index_In_Path + 1 and then
                    State.Next_Segment_Index_In_Path = 0)));

private

end Waypoint_Plan_Manager;
