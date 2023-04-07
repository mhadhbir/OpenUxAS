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

   subtype Ext_Vector_Index is Natural range 0 .. Integer (Max);
   subtype Vector_Index is Natural range 0 .. Integer (Max);

   function Pos64_Hash (X : Pos64) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (X));
   package Pos64_WP_Maps is new SPARK.Containers.Formal.Hashed_Maps (Pos64, Waypoint, Pos64_Hash);
   use Pos64_WP_Maps;
   subtype Pos64_WP_Map is Pos64_WP_Maps.Map (Max, Pos64_WP_Maps.Default_Modulus (Max))
     with Predicate =>
              (for all Id of Pos64_WP_Map =>
                 (Element (Pos64_WP_Map, Id).Number = Id and
                      Element (Pos64_WP_Map, Id).NextWaypoint >= 0));

   package Pos64_Nat64_Maps is new SPARK.Containers.Formal.Hashed_Maps (Pos64, Nat64, Pos64_Hash);
   use Pos64_Nat64_Maps;
   subtype Pos64_Nat64_Map is Pos64_Nat64_Maps.Map (Max, Pos64_Nat64_Maps.Default_Modulus (Max));

   package Pos64_Vectors is new SPARK.Containers.Formal.Vectors (Positive, Pos64);
   subtype Pos64_Vector is Pos64_Vectors.Vector (Max + 1);
   package Pos_Vec_M renames Pos64_Vectors.Formal_Model.M;
   use Pos64_Vectors;

   use Pos64_WP_Maps.Formal_Model;
   use Pos64_Nat64_Maps.Formal_Model;
   use Pos64_Vectors.Formal_Model;

   function Successor (M : Pos64_Nat64_Map; K : Pos64) return Nat64
                       renames Element;

   function Successor (M : Pos64_Nat64_Maps.Formal_Model.M.Map; K : Pos64) return Nat64
                       renames Pos64_Nat64_Maps.Formal_Model.Element;

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
      Path : Pos64_Vector;  -- Ordered list of waypoint Ids, built using info in Id_To_Next_Id, starting from MC.FirstWaypoint
      Cycle_Index : Ext_Vector_Index; -- If MC.WaypointList has a cycle, this is the index in Path that forms a cycle with the the last element
      Segment : Pos64_Vector; -- Next segment
      Next_First_Id : Nat64;  -- The waypoint Id to use for FirstWaypoint of the next segment
      Next_Segment_Index : Ext_Vector_Index; -- Index in Path for start of segment after this one
      New_Command : Boolean; -- Whether the most recent MissionCommand has yet -- been used to produce a segment
      Headed_To_First_Id : Boolean := False; -- Whether vehicle has reached -- FirstWaypoint of next segment
   end record;

   function Waypoints_Are_Subset
     (Id_To_Waypoint : Pos64_WP_Map;
      WaypointList : WP_Seq) return Boolean
   is
     (for all Id of Model (Id_To_Waypoint) =>
           Contains (WaypointList, WP_Sequences.First, Last (WaypointList),
        Element (Model (Id_To_Waypoint), Id)))
   with Ghost, Global => null;

   function Has_Same_Keys
     (M : Pos64_WP_Map;
      N : Pos64_Nat64_Map) return Boolean
   is
     ((for all I of Model (M) => Contains (Model (N), I)) and then
        (for all I of Model (N) => Contains (Model (M), I)))
   with Ghost, Global => null;

   function Elements_Are_Unique
     (V : Pos64_Vector) return Boolean
   is
     (for all I in Pos_Vec_M.First .. Pos_Vec_M.Last (Model (V)) =>
        (for all J in Pos_Vec_M.First .. Pos_Vec_M.Last (Model (V)) =>
             (if I /= J then
                   Element (Model (V), I) /= Element (Model (V), J))))
     with Ghost, Global => null;

   function Id_Keys_Match_Waypoint_Ids
     (Id_To_Next_Id : Pos64_Nat64_Map;
      Id_To_Waypoint : Pos64_WP_Map) return Boolean
   is
     (for all Id of Model (Id_To_Next_Id) =>
           Contains (Model (Id_To_Waypoint), Id) and then
      Element (Model (Id_To_Next_Id), Id) =
        Element (Model (Id_To_Waypoint), Id).NextWaypoint)
   with Ghost, Global => null;

   function Is_Subsegment
     (Path : Pos64_Vector;
      Cycle_Index : Vector_Index;
      Path_Index : Vector_Index;
      Segment : Pos64_Vector) return Boolean
   is
     (for all I in 1 .. Last_Index (Segment) =>
          (if Path_Index + I - 1 <= Last_Index (Path) then
              Element (Segment, I) = Element (Path, I + Path_Index - 1)
           else
              Element (Segment, I) =
                 Element (Path, (Path_Index + I - 1 - Last_Index (Path) - 1) mod
                    (Last_Index (Path) - Cycle_Index + 1) + Cycle_Index)))
   with
     Ghost,
     Pre =>
       Last_Index (Path) <= Integer (Max) and
       Cycle_Index in 1 .. Last_Index (Path) - 1 and
       Path_Index in 1 .. Last_Index (Path) and
       Last_Index (Segment) <= Integer (Max);

   function Elements_Are_Successors
     (Id_To_Next_Id : Pos64_Nat64_Map;
      Path : Pos64_Vector) return Boolean
   is
     (for all I in Pos_Vec_M.First .. Pos_Vec_M.Last (Model (Path)) - 1 =>
         Successor (Model (Id_To_Next_Id), Element (Model (Path), I)) =
        Element (Model (Path), I + 1))
   with
     Ghost,
     Pre =>
       (for all Id of Model (Path) =>
          Contains (Id_To_Next_Id, Id));

   function FirstWaypoint_Is_First_Or_Second_Element
     (FirstWaypoint : Pos64;
      Path : Pos64_Vector) return Boolean
   is
     (Element (Model (Path), 1) = FirstWaypoint or else
        (Length (Path) > 1 and then
         Element (Model (Path), 2) = FirstWaypoint))
   with
     Ghost, Pre => Length (Path) > 0;

   function Last_Element_Forms_Cycle
     (Id_To_Next_Id : Pos64_Nat64_Map;
      Path : Pos64_Vector) return Boolean
   is
     (Successor (Id_To_Next_Id, Last_Element (Path)) /= 0 and then
      Contains (Path, Successor (Id_To_Next_Id, Last_Element (Path))) and then
      Successor (Id_To_Next_Id, Last_Element (Path)) /= Last_Element (Path))
   with
     -- Ghost,
     Pre =>
       Length (Path) > 0 and then
       (for all Id of Model (Path) =>
          Contains (Model (Id_To_Next_Id), Id));

   function Cycle_Index_Is_Valid
     (Cycle_Index : Vector_Index;
      Path : Pos64_Vector;
      Id_To_Next_Id : Pos64_Nat64_Map) return Boolean
   is
     (Cycle_Index in 1 .. Last_Index (Path) - 1 and then
      Element (Path, Cycle_Index) =
          Successor (Id_To_Next_Id, Last_Element (Path)))
   with
     Ghost,
     Pre =>
       Length (Path) > 0 and then
       (for all Id of Model (Path) =>
          Contains (Model (Id_To_Next_Id), Id));

   procedure Handle_MissionCommand
     (State : in out Waypoint_Plan_Manager_State;
      MC : MissionCommand)
     with
       Pre =>
         Length (MC.WaypointList) <= Max and then
         MC.FirstWaypoint > 0,
       Post =>
         State.MC = MC and then
         Waypoints_Are_Subset (State.Id_To_Waypoint, State.MC.WaypointList) and then
         Has_Same_Keys (State.Id_To_Waypoint, State.Id_To_Next_Id) and then
         Id_Keys_Match_Waypoint_Ids (State.Id_To_Next_Id, State.Id_To_Waypoint) and then
         (for all Id of Model (State.Path) => Contains (State.Id_To_Next_Id, Id)) and then
         Elements_Are_Successors (State.Id_To_Next_Id, State.Path) and then
         Elements_Are_Unique (State.Path) and then
         (if not Contains (Model (State.Id_To_Next_Id), MC.FirstWaypoint) then
            State.Next_Segment_Index = 0 and State.Next_First_Id = 0 and
            Is_Empty (State.Path) and State.Cycle_Index = 0
          else
            State.Next_Segment_Index = 1 and then
            State.Next_First_Id = MC.FirstWaypoint and then
            FirstWaypoint_Is_First_Or_Second_Element
              (MC.FirstWaypoint, State.Path) and then
            (if Last_Element_Forms_Cycle (State.Id_To_Next_Id, State.Path) then
               Cycle_Index_Is_Valid (State.Cycle_Index, State.Path, State.Id_To_Next_Id)
             else
               State.Cycle_Index = 0));

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
         State.MC.FirstWaypoint > 0 and then
         Length (State.Path) > 0 and then
         Last_Index (State.Path) <= Positive (Max) and then
         Elements_Are_Unique (State.Path) and then
         State.Next_Segment_Index in 1 .. Last_Index (State.Path) and then
         State.Cycle_Index in 0 .. Last_Index (State.Path) - 1 and then
         (if State.New_Command then
            State.Next_Segment_Index = 1 and then
            State.Next_First_Id = State.MC.FirstWaypoint and then
            FirstWaypoint_Is_First_Or_Second_Element
              (State.MC.FirstWaypoint, State.Path)
            --  (Element (Model (State.Path), 1) = State.MC.FirstWaypoint or else
            --     (Length (State.Path) > 1 and then
            --      Element (Model (State.Path), 2) = State.MC.FirstWaypoint))
          else
            (if State.Cycle_Index = 0 then
               State.Next_Segment_Index < Last_Index (State.Path) and then
               Element (Model (State.Path), State.Next_Segment_Index + 1) =
                 State.Next_First_Id
             else
               (if State.Next_Segment_Index < Last_Index (State.Path) then
                  Element (Model (State.Path), State.Next_Segment_Index + 1) =
                    State.Next_First_Id
                else
                  Element (Model (State.Path), State.Cycle_Index) =
                    State.Next_First_Id))) and then
         (for all Id of Model (State.Path) => Contains (State.Id_To_Waypoint, Id)),
         Post =>
           State'Old.MC = State.MC and then
           State'Old.Id_To_Waypoint = State.Id_To_Waypoint and then
           State'Old.Id_To_Next_Id = State.Id_To_Next_Id and then
           State'Old.Path = State.Path and then
           State'Old.Cycle_Index = State.Cycle_Index and then
           State'Old.Headed_To_First_Id = State.Headed_To_First_Id and then
           State.New_Command = False and then
           Element (Model (State.Segment), 1) =
             Element (Model (State.Path), State'Old.Next_Segment_Index) and then
           (for all Id of Model (State.Segment) => Pos_Vec_M.Contains (Model (State.Path), 1, Pos_Vec_M.Last (Model (State.Path)), Id)) and then
           (if State.Cycle_Index > 0 then
              Positive (Length (State.Segment)) = Integer (Config.NumberWaypointsToServe) and then
              Is_Subsegment (State.Path, State.Cycle_Index, State.Next_Segment_Index'Old, State.Segment) and then
              State.Next_Segment_Index in 1 .. Pos_Vec_M.Last (Model (State.Path)) and then
              Element (Model (State.Segment), Pos_Vec_M.Last (Model (State.Segment)) - Integer (Config.NumberWaypointsOverlap) + 1) =
              Element (Model (State.Path), State.Next_Segment_Index) and then
              (if State.Next_Segment_Index < Last_Index (State.Path) then
                 State.Next_First_Id = Element (Model (State.Path), State.Next_Segment_Index + 1)
               else
                 State.Next_First_Id = Element (Model (State.Path), State.Cycle_Index))
            else
              (for all I in 1 .. Integer (Pos_Vec_M.Length (Model (State.Segment))) =>
               Element (Model (State.Segment), I) = Element (Model (State.Path), State.Next_Segment_Index'Old + I - 1)) and then
              (if Positive (Pos_Vec_M.Length (Model (State.Path))) - State.Next_Segment_Index'Old + 1 >= Integer (Config.NumberWaypointsToServe)
               then
                 Positive (Length (State.Segment)) = Integer (Config.NumberWaypointsToServe) and then
                 (if Last_Index (State.Path) = Last_Index (State.Segment)
                  then
                      State.Next_Segment_Index = 0 and then
                      State.Next_First_Id = 0
                  else
                    State.Next_First_Id = Element (Model (State.Path), State.Next_Segment_Index + 1) and then
                    State.Next_Segment_Index = State.Next_Segment_Index'Old + Integer (Config.NumberWaypointsToServe) - Integer (Config.NumberWaypointsOverlap) and then
                    (Element (Model (State.Segment), Pos_Vec_M.Last (Model (State.Segment)) - Integer (Config.NumberWaypointsOverlap) + 1) =
                       Element (Model (State.Path), State.Next_Segment_Index)))
                else
                  Positive (Length (State.Segment)) = Positive (Length (State.Path)) - State.Next_Segment_Index'Old + 1 and then
                 State.Next_Segment_Index = 0));

   procedure Lemma_Map_Still_Contains_List_After_Append
     (M : Pos64_Nat64_Map;
      L_Old, L_New : Pos64_Vector;
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
     (V_Old, V_New : Pos64_Vector;
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
      L_Old, L_New : Pos64_Vector;
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

private

end Waypoint_Plan_Manager;
