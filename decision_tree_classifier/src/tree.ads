
--  Based on scikit-learn/sklearn/tree _tree.pxd class Tree
--  pxd files are like C header files.
--  They contain Cython declarations and, sometimes, code sections that
--  are only meant for inclusion in Cython modules.
--  The Tree object is a binary tree structure constructed by the TreeBuilder.
--  The tree structure is used for predictions and feature importances.

with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Vectors;

with Classifier_Types;
with ML_Types;

package Tree is

   type State is (None);
   type Data_Type is (Integer_Type, Float_Type, Enum_Type);
   type Feature_Type is (No_Feature, Auto_Feature, Sqrt_Feature, Log2_Feature);
   type Value_Array (<>) is private;

   Max_Array_Size : constant Integer := 4000;
   type Index_Range is range 1 .. Max_Array_Size;

   type Features_Record (Feature_Kind : Data_Type) is record
      case Feature_Kind is
         when Integer_Type =>  Max_Features     : Integer := 0;
         when Float_Type   => Fraction_Features : Float := 0.0;
         when Enum_Type    => Max_Featue_Type   : Feature_Type := No_Feature;
      end case;
   end record;

   subtype Values_List is Classifier_Types.Float_List;
   subtype List_Of_Values_Lists is Classifier_Types.List_Of_Float_Lists;

   type Tree_Node (Leaf_Node : Boolean := False) is record
      --  from _Tree Node struct
      Impurity                  : Float := Float'Large;
      Num_Node_Samples          : Positive := 1;
      Weighted_Num_Node_Samples : Natural := 0;
      --  From Tree Utils StackRecord struct
      Samples_Start             : Positive := 1;
      Depth                     : Positive := 1;
      Is_Left                   : Boolean := True;
      Num_Constant_Features     : Integer := 0;
      Values                    : List_Of_Values_Lists;
      case Leaf_Node is
         when False =>
            --  from _Tree Node struct
            --  Feature used for splitting the node
            Feature_Index : Positive := 1;
            Threshold     : Float := 0.0;
         when True => null;
      end case;
   end record;

   package Nodes_Package is new Ada.Containers.Indefinite_Multiway_Trees
     (Tree_Node);
   subtype Tree_Nodes is Nodes_Package.Tree;
   subtype Tree_Cursor is Nodes_Package.Cursor;
   type Leaf_Cursor_Array is array (Integer range <>) of Tree_Cursor;

   use Nodes_Package;
   package Tree_Cursor_Package is new Ada.Containers.Vectors
     (Positive, Tree_Cursor);
   subtype Tree_Cursor_List is Tree_Cursor_Package.Vector;

   type Tree_Attributes is private;
   type Tree_Class is record
      Num_Features    : Natural := 0;
      Classes         : ML_Types.List_Of_Value_Data_Lists;
      Num_Outputs     : Index_Range := 1;
      Max_Depth       : Integer := -1;
      Nodes           : Nodes_Package.Tree;  -- Ada Multiway Tree
      Attributes      : Tree_Attributes;
   end record;

   Value_Error : Exception;

   function Apply (Self : Tree_Class;
                   X    : ML_Types.List_Of_Value_Data_Lists)
                   return Leaf_Cursor_Array;
   --     procedure Fit moved to fit_functions
   --     procedure Fit (Self          : Validation.Attribute_List;
   --                    X, Y          : Sample_Matrix;
   --                    Sample_Weight : State := None;
   --                    Check_Input   : Boolean := True;
   --                    X_Idx_Sorted  : State := None);
--     function Get_Value_Array (Self : Tree_Class) return Value_Array;
   function Predict (Self : Tree_Class;
                     X    : ML_Types.List_Of_Value_Data_Lists)
                     return ML_Types.Value_Data_List;

private

   type Tree_Attributes is record
      Node_Count : Natural := 0;
      Max_Depth  : Natural := 0;
   end record;

   type Value_Array is array
     (Natural range <>) of Float;

end Tree;
