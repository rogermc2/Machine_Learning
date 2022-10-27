
with Interfaces.C; use Interfaces.C;

with API_Vectors_Matrices; use API_Vectors_Matrices;
with Node_Splitter;

package body Py.Classification is
   pragma Warnings (Off, "not referenced");

   Max_Length : constant size_t := 1024;

   type Unsigned_Array is array (Positive range <>) of access unsigned;
   pragma Convention (C, Unsigned_Array);

   type Vector3_Array is array (Positive range <>)
     of access API_Vectors_Matrices.API_Vector_3D;
   pragma Convention (C, Vector3_Array);

   --  This declaration has been checked OK for Key data. DON'T CHANGE
   type API_String is record
      Length  : size_t := 0;
      Data    : char_array (0 .. Max_Length - 1) := (others => Interfaces.C.char'Val (0));
   end record;
   pragma Convention (C_Pass_By_Copy, API_String);

   type API_Splitter_Class is record
      Criteria             : Criterion.Criterion_Class;
      Max_Features         : unsigned := 1;  --  Number of features to test
      Min_Leaf_Samples     : unsigned := 0;
      Min_Leaf_Weight      : C_float := 0.0;
      --  Samples:
      Sample_Indices       : access Unsigned_Array;
      Feature_Indices      : access Unsigned_Array;
      Constant_Features_I  : access Unsigned_Array;
      Num_Classes          : access Unsigned_Array;
      Feature_Values       : access API_Vector_2D_Array;
      Num_Samples          : unsigned := 0;
      Weighted_Samples     : C_float := 0.0;
      --  encoded version of sample Y
--        Y_Encoded            : Natural_Lists_2D;
      Sample_Weight        : Weights.Weight_List;
      Node_Impurity        : C_float := C_float (-Float'Last);
      Start_Row            : unsigned := 1;
      Stop_Row             : unsigned := 1;
      --  BaseDenseSplitter elements
--        X                    : ML_Types.Value_Data_Lists_2D;
      Total_Samples        : unsigned := 0;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Splitter_Class);

   type API_Classifier is record
      --  Parameters
      Criterion_Kind           : Criterion.Classifier_Criteria_Type :=
                                   Criterion.Gini_Criteria;
      Splitter_Kind            : Splitter_Type := Best_Splitter;
      Splitter                 : Node_Splitter.Splitter_Class;
      Min_Samples_Split        : Split_Value_Record := Default_Min_Split;
      Max_Depth                : Interfaces.C.int := -1;  --  < 0 means unspecified
      Min_Samples_Leaf         : Interfaces.C.int := 1;
      Min_Weight_Fraction_Leaf : Interfaces.C.C_float := 0.0;
      Max_Features             : Tree.Index_Range := 1;
      Random_State             : Interfaces.C.int := 0;
      Max_Leaf_Nodes           : Interfaces.C.int := -1;  --  < 0 means unspecified
      --  Impure means that data is mixture of different classes.
      Min_Impurity_Decrease    : Interfaces.C.C_float := 0.0;
      Min_Impurity_Split       : Interfaces.C.C_float := 0.0;
      Class_Weight             : Weights.Weight_Type := Weights.No_Weight;
      Presort                  : String (1 .. 10) := "Deprecated";
      CCP_Alpha                : Interfaces.C.C_float := 0.0;
      --  Attributes
      --  The classes labels (single output problem)
      --  or a list of arrays of class labels (multi-output problem).
--        Classes             : ML_Types.Value_Data_Lists_2D;
      --  The impurity-based feature importances.
      --  The higher, the more important the feature.
      Feature_Importances : Unbounded_List;
      Num_Features        : unsigned := 1;
      Num_Outputs         : unsigned := 1;
--        Decision_Tree       : Tree.Tree_Class;
   end record;
   pragma Convention (C_Pass_By_Copy, API_Classifier);

   procedure Create_Classifier
     (Name                     : in out Classifier;
      Min_Samples_Split        : String :="";
      Criterion_Type           : Criterion.Classifier_Criteria_Type :=
        Criterion.Gini_Criteria;
      Min_Leaf_Samples         : Integer := 1;
      Max_Features             : Tree.Index_Range :=
        Tree.Index_Range'Last;
      Class_Weight             : Weights.Weight_Type := Weights.No_Weight;
      Max_Depth                : Integer := -1;
      Min_Weight_Fraction_Leaf : Float := 0.0;
      Max_Leaf_Nodes           : Integer := -1;
      Min_Impurity_Decrease    : Float := 0.0;
      CCP_Alpha                : Float := 0.0;
      Random_State             : Integer := 0) is
      Module_Name   : aliased char_array := "sample_module" & Nul;
      aClassifier   : Classifier
        (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
   begin
      null;

   end Create_Classifier;

end Py.Classification;
