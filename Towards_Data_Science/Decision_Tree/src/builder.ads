
with Ada.Containers.Indefinite_Multiway_Trees;

with Support;

package Builder is

   --     type Decision_Node_Type;
   --     type R_Tree is access Decision_Node_Type;
   --     Null_Tree: constant R_Tree:= null;
   --     type Decision_Node_Type is record
   --        Question      : Support.Question_Type;
   --        Left_Subtree  : R_Tree;
   --        Right_Subtree : R_Tree;
   --        Counts        : Support.Count_Package.Map
   --          := Support.Count_Package.Empty_Map;
   --     end record;
   type Node_Kind is (Decision_Kind, Prediction_Kind);

   type Partitioned_Rows is record
      True_Rows  : Support.Rows_Vector;
      False_Rows : Support.Rows_Vector;
   end record;

   type Best_Split_Data is record
      Best_Gain     : Float;
      Best_Question : Support.Question_Type;
   end record;

   --     type Decision_Node_Type is record
   --        Question : Support.Question_Type;
   --        Rows     : Support.Rows_Vector := Support.Rows_Package.Empty_Vector;
   --        Counts   : Support.Count_Package.Map := Support.Count_Package.Empty_Map;
   --     end record;
   --
   type Decision_Node_Type (Node_Type : Node_Kind := Decision_Kind) is record
      case Node_Type is
      when  Decision_Kind =>
         Question    : Support.Question_Type;
         True_Rows   : Support.Rows_Vector := Support.Rows_Package.Empty_Vector;
         False_Rows  : Support.Rows_Vector := Support.Rows_Package.Empty_Vector;
      when Prediction_Kind =>
         Predictions : Support.Count_Package.Map := Support.Count_Package.Empty_Map;
      end case;
   end record;

      package Tree_Package is new Ada.Containers.Indefinite_Multiway_Trees
        (Decision_Node_Type);
      subtype Tree_Cursor is Tree_Package.Cursor;
      subtype Tree_Type is Tree_Package.Tree;

      function Build_Tree (Rows : Support.Rows_Vector) return Tree_Type;
      function Classify (aRow : Support.Row_Data; aTree : Tree_Type)
                         return Support.Count_Package.Map;
      function Find_Best_Split (Rows : Support.Rows_Vector) return Best_Split_Data;
      function Match (Self    : Support.Question_Type;
                      Example : Support.Row_Data) return Boolean;
      function Partition (Rows     : Support.Rows_Vector;
                          Question : Support.Question_Type)
                       return Partitioned_Rows;
      procedure Print_Tree1 (Node : Decision_Node_Type);
      procedure Print_Tree (aTree : Tree_Type);

end Builder;
