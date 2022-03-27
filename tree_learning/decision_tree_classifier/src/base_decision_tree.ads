
--  Based on scikit-learn/sklearn/tree tree _classes.py
--  class BaseDecisionTree(MultiOutputMixin, BaseEstimator)
--  Decision Trees (DTs) are a non-parametric supervised learning method used
--  for classification and regression.
--  The goal is to create a model that predicts the value of a target variable
--  by learning simple decision rules inferred from the data features.

--  The default values for the parameters controlling the size of the trees
--  (e.g. ``max_depth``, ``min_samples_leaf``, etc.) lead to fully grown and
--  unpruned trees that can be very large on some data sets.
--  To reduce memory consumption the complexity and size of the trees should be
--  controlled by setting those parameter values.

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;
with Tree;

with Classifier_Types;
with Criterion;
with Estimator;
with Node_Splitter;
with Weights;

package Base_Decision_Tree is
   --  Gini Impurity is a measurement of the likelihood of an incorrect
   --  classification of a new instance of a random variable.
   type Splitter_Type is (Best_Splitter, Random_Splitter);
   Type State is (None);
   Type Weight_Type is (None);

   package UB_Package is new Ada.Containers.Vectors
     (Positive, Unbounded_String);
   subtype Unbounded_List is UB_Package.Vector;

   type Leaf_Record (Leaf_Type : Tree.Data_Type) is record
      case Leaf_Type is
         when Tree.Integer_Type =>  Min_Leaf       : Integer := 1;
         when Tree.Float_Type => Min_Fraction_Leaf : Float := 1.0;
         when Tree.Enum_Type => null;
      end case;
   end record;

   type Max_Features_Type is (Max_Features_Int, Max_Features_Float,
                              Max_Features_Auto, Max_Features_Sqrt,
                              Max_Features_Log_Sq, Max_Features_None);

   type Split_Data_Type is (Split_Integer, Split_Float);

   type Split_Value_Record (Value_Kind : Split_Data_Type
                            := Split_Integer) is record
      case Value_Kind is
         when Split_Float => Float_Value     : Float := 0.0;
         when Split_Integer => Integer_Value : Integer := 2;
      end case;
   end record;

   Default_Min_Split : Split_Value_Record (Split_Integer);

   --  class BaseDecisionTree
   type Base_Parameter_Data
     (Split_Type, Leaf_Type, Feature_Type : Tree.Data_Type) is record
      Criterion_Kind           : Criterion.Classifier_Criteria_Type :=
                                   Criterion.Gini_Criteria;
      Splitter_Kind            : Splitter_Type := Best_Splitter;
      Splitter                 : Node_Splitter.Splitter_Class;
      Min_Samples_Split        : Split_Value_Record := Default_Min_Split;
      Max_Depth                : Integer := -1;  --  < 0 means unspecified
      Min_Samples_Leaf         : Integer := 1;
      Min_Weight_Fraction_Leaf : Float := 0.0;
      Max_Features             : Tree.Index_Range := 1;
      Random_State             : Integer := 0;
      Max_Leaf_Nodes           : Integer := -1;  --  < 0 means unspecified
      --  Impure means that data is mixture of different classes.
      Min_Impurity_Decrease    : Float := 0.0;
      Min_Impurity_Split       : Float := 0.0;
      Class_Weight             : Weights.Weight_Type := Weights.No_Weight;
      Presort                  : String (1 .. 10) := "Deprecated";
      CCP_Alpha                : Float := 0.0;
   end record;

   --  class DecisionTree parameters additional to Base_Parameter_Data
   type Attribute_Data is record
      --  The classes labels (single output problem)
      --  or a list of arrays of class labels (multi-output problem).
      Classes             : ML_Types.Value_Data_Lists_2D;
      --  The impurity-based feature importances.
      --  The higher, the more important the feature.
      Feature_Importances : Unbounded_List;
      Max_Features        : Positive := 1;
      Num_Features        : Tree.Index_Range := 1;
      Num_Outputs         : Tree.Index_Range := 1;
      Decision_Tree       : Tree.Tree_Class;
   end record;

   --  class DecisionTreeClassifier
   type Classifier
     (Split_Type, Leaf_Type, Feature_Type : Tree.Data_Type) is record
      Estimator_Kind : Estimator.Estimator_Type :=
                         Estimator.Classifier_Estimator;
      Parameters     : Base_Parameter_Data (Split_Type, Leaf_Type, Feature_Type);
      Attributes     : Attribute_Data;
   end record;

   Classifier_Error : Exception;
   Value_Error      : Exception;

   function Apply (aClassifier : Classifier; X : ML_Types.Value_Data_Lists_2D)
                   return Classifier_Types.Natural_List;
   --  BaseDecisionTree.Fit (super of class DecisionTreeClassifier)
   --  Fit builds a decision tree classifier from the training set (X, Y).
   --  X : training input samples; a (n_samples, n_features) matrix
   --  Y : values (class labels); a (n_samples, n_outputs) matrix
   procedure Base_Fit
     (aClassifier    : in out Classifier;
      X              : ML_Types.Value_Data_Lists_2D;
      Y_Orig         : ML_Types.Value_Data_Lists_2D;
      Sample_Weights : out Weights.Weight_List);
   procedure C_Init
     (aClassifier              : in out Classifier;
      Min_Samples_Split        : String;
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
      Random_State             : Integer := 0);
   function Predict (Self : in out Classifier;
                     X    : ML_Types.Value_Data_Lists_2D)
                     return ML_Types.Value_Data_Lists_2D;
   function Decision_Path (aClassifier : Classifier;
                           X           : ML_Types.Value_Data_Lists_2D)
                           return Classifier_Types.Natural_Lists_2D;

end Base_Decision_Tree;