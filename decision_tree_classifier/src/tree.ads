
--  Based on scikit-learn/sklearn/tree _tree.pxd class Tree
--  pxd files are like C header files.
--  They contain Cython declarations and, sometimes, code sections that
--  are only meant for inclusion in Cython modules.
--  The Tree object is a binary tree structure constructed by the TreeBuilder.
--  The tree structure is used for predictions and feature importances.

with Ada.Containers.Indefinite_Multiway_Trees;

with Classifier_Types; use Classifier_Types;
with ML_Types;
--  with Validation;

package Tree is

   type State is (None);
   type Data_Type is (Integer_Type, Float_Type, Enum_Type);
   type Feature_Type is (No_Feature, Auto_Feature, Sqrt_Feature, Log2_Feature);

   Max_Array_Size : constant Integer := 4000;
   type Index_Range is range 1 .. Max_Array_Size;

   type Features_Record (Feature_Kind : Data_Type) is record
      case Feature_Kind is
         when Integer_Type =>  Max_Features     : Integer := 0;
         when Float_Type   => Fraction_Features : Float := 0.0;
         when Enum_Type    => Max_Featue_Type   : Feature_Type := No_Feature;
      end case;
   end record;

   type Tree_Node (Feature_Sel : Data_Type) is record
--        Left_Child_ID             : Integer := 0;
--        Right_Child_ID            : Integer := 0;
      --  Feature used for splitting the node
      Feature                   : Features_Record (Feature_Sel);
      Num_Node_Samples          : Integer := 0;
      Weighted_Num_Node_Samples : Integer := 0;
      Depth                     : Integer := 0;
      --        Parent                    : Tree_Node;
      Is_Left                   : Integer := 0;
      Threshold                 : Float := 0.0;
      Impurity                  : Float := Float'Large;  --  "Infinity"
      Num_Constant_Features     : Integer := 0;
   end record;

   package Tree_Package is new Ada.Containers.Indefinite_Multiway_Trees
     (Tree_Node);
   --     type Tree_List is new Tree_Package.Vector with null record;
   type Values_Array is array
     (Index_Range range <>, Index_Range range <>, Index_Range range <>) of float;

   type Tree_Attributes is private;
   type Tree_Class (Capacity, Num_Outputs, Max_Num_Classes : Index_Range := 1)
   is record
      Num_Features    : Natural := 0;
      Classes         : ML_Types.Value_Data_List;
      Max_Depth       : Natural := 0;
      Node_Count      : Natural := 0;
      Nodes           : Tree_Package.Tree;  -- Ada Multiway Tree
      --  Values declaration causes runtime problem
--        Values          : Values_Array
--          (1 .. Capacity, 1 .. Num_Outputs, 1 .. Max_Num_Classes)
--          := (others => (others => (others => 0.0)));
      --        Value_Stride    : Integer := Integer (Num_Outputs * Max_Num_Classes);
      Attributes      : Tree_Attributes;
   end record;

   Value_Error : Exception;

   --     procedure Fit moved to fit_functions
   --     procedure Fit (Self          : Validation.Attribute_List;
   --                    X, Y          : Sample_Matrix;
   --                    Sample_Weight : State := None;
   --                    Check_Input   : Boolean := True;
   --                    X_Idx_Sorted  : State := None);

   --  Predict class probabilities of the input samples X.
   --  The predicted class probability is the fraction of samples of the same
   --   class in a leaf.
   --     function Predict_Probability (Self : Validation.Attribute_List;
   function Predict_Probability (X           : Sample_Matrix;
                                 Check_Input : Boolean := True)
                                 return Probabilities_List;
   --  Predict class log-probabilities of the input samples X.
   --     function Predict_Log_Probability (Self : Validation.Attribute_List;
   function Predict_Log_Probability (X    : Sample_Matrix)
                                     return Probabilities_List;
   procedure Resize (Self : in out Tree_Class; Capacity : Positive);
private

   type Tree_Attributes is record
      Node_Count : Natural := 0;
      Capacity   : Natural := 0;
      Max_Depth  : Natural := 0;
   end record;

end Tree;
