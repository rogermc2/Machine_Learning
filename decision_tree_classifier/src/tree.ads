
--  Based on scikit-learn/sklearn/tree _tree.pxd class Tree
--  pxd files are like C header files.
--  They contain Cython declarations and, sometimes, code sections that
--  are only meant for inclusion in Cython modules.
--  The Tree object is a binary tree structure constructed by the TreeBuilder.
--  The tree structure is used for predictions and feature importances.

with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Vectors;

with ML_Types;

package Tree is

   type State is (None);
   type Data_Type is (Integer_Type, Float_Type, Enum_Type);
   type Feature_Type is (No_Feature, Auto_Feature, Sqrt_Feature, Log2_Feature);
   type Out_Array is array (Integer range <>) of Positive;

   Max_Array_Size : constant Integer := 4000;
   type Index_Range is range 1 .. Max_Array_Size;

   type Features_Record (Feature_Kind : Data_Type) is record
      case Feature_Kind is
         when Integer_Type =>  Max_Features     : Integer := 0;
         when Float_Type   => Fraction_Features : Float := 0.0;
         when Enum_Type    => Max_Featue_Type   : Feature_Type := No_Feature;
      end case;
   end record;

   type Tree_Node
     (Is_Leaf : Boolean := False)
   is record
      Node_Index                : Natural := 0;
      Kind                      : ML_Types.Node_Kind := ML_Types.Undefined_Node;
      Num_Node_Samples          : Integer := 0;
      Weighted_Num_Node_Samples : Integer := 0;
      Depth                     : Integer := 0;
      Is_Left                   : Boolean := True;
      Impurity                  : Float := Float'Large;  --  "Infinity"
      Num_Constant_Features     : Integer := 0;
      case Is_Leaf is
         when False =>
            --  Feature used for splitting the node
            Feature_Index : Positive := 1;
            Threshold     : Float := 0.0;
         when True => null;
      end case;
   end record;

   package Tree_Package is new Ada.Containers.Indefinite_Multiway_Trees
     (Tree_Node);
   subtype Tree_Type is Tree_Package.Tree;
   subtype Tree_Cursor is Tree_Package.Cursor;

   use Tree_Package;
   package Tree_Cursor_Package is new Ada.Containers.Vectors
     (Positive, Tree_Cursor);
   subtype Tree_Cursor_List is Tree_Cursor_Package.Vector;

   package Class_Package is new Ada.Containers.Vectors
     (Positive, Float);
   subtype Class_List is Class_Package.Vector;

   use Class_Package;
   package Output_Package is new Ada.Containers.Vectors
     (Positive, Class_List);
   subtype Output_List is Output_Package.Vector;

   use Output_Package;
   package Values_Package is new Ada.Containers.Vectors
     (Positive, Output_List);
   subtype Values_List is Values_Package.Vector;

   type Tree_Attributes is private;
   type Tree_Class is record
      Num_Features    : Natural := 0;
      Classes         : ML_Types.Value_Data_List;
      --        Capacity        : Index_Range := 1;
      Num_Outputs     : Index_Range := 1;
      --        Max_Num_Classes  : Index_Range := 1;
      Max_Depth       : Natural := 0;
      Node_Count      : Natural := 0;
      Nodes           : Tree_Package.Tree;  -- Ada Multiway Tree
      Values          : Values_List;
      Attributes      : Tree_Attributes;
   end record;

   Value_Error : Exception;

   function Apply (Self : Tree_Class;
                   X    : ML_Types.List_Of_Value_Data_Lists)
                   return Out_Array;
   --     procedure Fit moved to fit_functions
   --     procedure Fit (Self          : Validation.Attribute_List;
   --                    X, Y          : Sample_Matrix;
   --                    Sample_Weight : State := None;
   --                    Check_Input   : Boolean := True;
   --                    X_Idx_Sorted  : State := None);

   function Predict (Self : Tree_Class;
                     X    : ML_Types.List_Of_Value_Data_Lists)
                     return ML_Types.Value_Data_List;

private

   type Tree_Attributes is record
      Node_Count : Natural := 0;
      Max_Depth  : Natural := 0;
   end record;

end Tree;
