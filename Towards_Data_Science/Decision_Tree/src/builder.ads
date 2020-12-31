
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types; use ML_Types;

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
      True_Rows  : Rows_Vector;
      False_Rows : Rows_Vector;
   end record;

   type Best_Split_Data is record
      Best_Gain     : Float;
      Best_Question : Question_Type;
   end record;

   --     type Decision_Node_Type is record
   --        Question : Question_Type;
   --        Rows     : Rows_Vector := Rows_Package.Empty_Vector;
   --        Counts   : Count_Package.Map := Count_Package.Empty_Map;
   --     end record;
   --
   type Decision_Node_Type (Node_Type : Node_Kind := Decision_Kind) is record
      case Node_Type is
      when  Decision_Kind =>
         Question    : Question_Type;
         True_Rows   : Rows_Vector := Rows_Package.Empty_Vector;
         False_Rows  : Rows_Vector := Rows_Package.Empty_Vector;
      when Prediction_Kind =>
         Predictions : Count_Package.Map := Count_Package.Empty_Map;
      end case;
   end record;

   package Tree_Package is new Ada.Containers.Indefinite_Multiway_Trees
     (Decision_Node_Type);
   subtype Tree_Cursor is Tree_Package.Cursor;
   subtype Tree_Type is Tree_Package.Tree;

   package Strings_Package is new Ada.Containers.Doubly_Linked_Lists
     (Unbounded_String);
   subtype Strings_List is Strings_Package.List;

   function Build_Tree (Rows : Rows_Vector) return Tree_Type;
   function Classify (aRow : Row_Data; aTree : Tree_Type)
                         return Count_Package.Map;
   function Find_Best_Split (Rows : Rows_Vector) return Best_Split_Data;
   function Match (Self    : Question_Type;
                   Example : Row_Data) return Boolean;
   function Partition (Rows     : Rows_Vector;
                       Question : Question_Type)
                          return Partitioned_Rows;
   function Print_Leaf (Counts : Count_Package.Map)
                        return Strings_List;
   procedure Print_Tree1 (Node : Decision_Node_Type);
   procedure Print_Tree (aTree : Tree_Type);

end Builder;
