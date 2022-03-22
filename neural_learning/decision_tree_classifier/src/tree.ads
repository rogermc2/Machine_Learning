
--  Based on scikit-learn/sklearn/tree _tree.pxd class Tree
--  pxd files are like C header files.
--  They contain Cython declarations and, sometimes, code sections that
--  are only meant for inclusion in Cython modules.
--  The Tree object is a binary tree structure constructed by the TreeBuilder.
--  The tree structure is used for predictions and feature importances.

with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Vectors;

with NL_Types; use NL_Types;
with Weights;

package Tree is

   type State is (None);
   type Data_Type is (Integer_Type, Float_Type, Enum_Type);
   type Feature_Type is (No_Feature, Auto_Feature, Sqrt_Feature, Log2_Feature);
   type Node_Type is (Top_Node, Left_Node, Right_Node);

   Max_Array_Size : constant Integer := 4000;
   type Index_Range is range 1 .. Max_Array_Size;

   type Features_Record (Feature_Kind : Data_Type) is record
      case Feature_Kind is
         when Integer_Type =>  Max_Features     : Integer := 0;
         when Float_Type   => Fraction_Features : Float := 0.0;
         when Enum_Type    => Max_Featue_Type   : Feature_Type := No_Feature;
      end case;
   end record;

   subtype Values_List is Float_List;
   subtype Values_List_2D is Float_List_2D;
   subtype Values_List_3D is Float_List_3D;

   type Tree_Node (Leaf_Node : Boolean := False) is record
      Node_ID                   : Positive := 1;
      --  from _Tree Node struct
      Impurity                  : Float := Float'Last;
      Num_Node_Samples          : Positive := 1;
      Weighted_Num_Node_Samples : Natural := 0;
      case Leaf_Node is
         when False =>
            --  from _Tree Node struct
            Features               : Unbounded_List;
            --  Feature used for splitting the node
            Best_Fit_Feature_Index : Natural := 0;
            Threshold              : Float := 0.0;
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

   package Nodes_List_Package is new Ada.Containers.Vectors
     (Positive, Tree_Node);
   subtype Nodes_List is Nodes_List_Package.Vector;

   type Tree_Class is record
      --  _tree.pxd L43
      Num_Features : Natural := 0;
      --  Classes:  outputs x classes
      Num_Classes  : Natural := 0;
      Classes      : Integer_List;
--        Num_Outputs  : Index_Range := 1;
      Max_Depth    : Integer := -1;
      Nodes        : Nodes_Package.Tree;  -- Ada Multiway Tree
      --  From _Treenp.ndarray _get_value_ndarray generates a Values
      --  2D array, num_nodes x num_classes per node.
      Values       : Weights.Weight_Lists_2D;
   end record;

   Value_Error : Exception;

   function Apply (Self : Tree_Class; X : Float_List_2D)
                   return Natural_List;
   procedure C_Init (aTree : in out Tree_Class; Num_Features : Natural := 0;
                     Num_Classes  : Natural:= 0);
   function Decision_Path (aTree : Tree_Class; X : Float_List_2D)
                           return Natural_List;
   --     procedure Fit moved to fit_functions
   --     procedure Fit (Self          : Validation.Attribute_List;
   --                    X, Y          : Sample_Matrix;
   --                    Sample_Weight : State := None;
   --                    Check_Input   : Boolean := True;
   --                    X_Idx_Sorted  : State := None);
--     function Predict (Self : in out Tree_Class; X : Float_List_2D)
--                       return Weights.Weight_Lists_3D;
   function Predict (Self : in out Tree_Class; X : Float_List_2D)
                     return Weights.Weight_Lists_2D;

end Tree;
