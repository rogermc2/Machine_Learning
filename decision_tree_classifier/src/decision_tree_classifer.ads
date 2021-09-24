
--  Based on scikit-learn/sklearn/tree tree _classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)
--  Decision Trees (DTs) are a non-parametric supervised learning method used
--  for classification and regression.
--  The goal is to create a model that predicts the value of a target variable
--  by learning simple decision rules inferred from the data features.

--  The default values for the parameters controlling the size of the trees
--  (e.g. ``max_depth``, ``min_samples_leaf``, etc.) lead to fully grown and
--  unpruned trees that can be very large on some data sets.
--  To reduce memory consumption the complexity and size of the trees should be
--  controlled by setting those parameter values.

--  Example:
--  clf : DecisionTreeClassifier (random_state=0);
--  clf := clf.fit (Data, Labels);

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;
with Tree;

with Classifier_Types; use Classifier_Types;
with Node_Splitter;
with Weights;

package Decision_Tree_Classifer is
   --  Gini Impurity is a measurement of the likelihood of an incorrect
   --  classification of a new instance of a random variable.
   type Classifier_Criteria_Type is (Gini_Criteria, Entropy_Criteria);
   type Regressor_Criteria_Type is (MSE_Criteria, Friedman_MSE_Criteria, MAE_Criteria);
   type Splitter_Type is (Best_Splitter, Random_Splitter);
   Type State is (None);
   Type Weight_Type is (None);

   package UB_Package is new Ada.Containers.Vectors
     (Positive, Unbounded_String);
   subtype Unbounded_List is UB_Package.Vector;

   type Split_Record (Split_Type : Tree.Data_Type) is record
      --  The minimum number of samples required to split an internal node.
      case Split_Type is
         when Tree.Integer_Type =>  Min_Split         : Integer := 2;
         when Tree.Float_Type   => Min_Fraction_Split : Float := 2.0;
         when Tree.Enum_Type    => null;
         when others    => null;
      end case;
   end record;

   type Leaf_Record (Leaf_Type : Tree.Data_Type) is record
      case Leaf_Type is
         when Tree.Integer_Type =>  Min_Leaf       : Integer := 1;
         when Tree.Float_Type => Min_Fraction_Leaf : Float := 1.0;
         when Tree.Enum_Type => null;
         when others => null;
      end case;
   end record;

   type Max_Features_Type is (Max_Features_Int, Max_Features_Float,
                              Max_Features_Auto, Max_Features_Sqrt,
                              Max_Features_Log_Sq, Max_Features_None);

   --  class BaseDecisionTree
   type Base_Parameter_Data
     (Split_Type, Leaf_Type, Feature_Type : Tree.Data_Type) is record
      Criterion                : Classifier_Criteria_Type := Gini_Criteria;
      Splitter_Kind            : Splitter_Type := Best_Splitter;
      Splitter                 : Node_Splitter.Splitter_Class;
      Max_Depth                : Integer := 0;
      Min_Samples_Split        : Split_Record (Split_Type);
      Min_Samples_Leaf         : Leaf_Record (Leaf_Type);
      Min_Weight_Fraction_Leaf : Float := 0.0;
      Max_Features             : Tree.Features_Record (Feature_Type);
      Random_State             : Integer := 0;
      Max_Leaf_Nodes           : Integer := -1;  --  < 0 means unsppecified
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
      Classes             : ML_Types.List_Of_Value_Data_Lists;
      --  The impurity-based feature importances.
      --  The higher, the more important the feature.
      Feature_Importances : Unbounded_List;
      Max_Features        : Natural := 0;
      --  The number of classes (for single output problems),
      --  or a list containing the number of classes for each
      --   output (for multi-output problems).
      Num_Classes         : Integer_List;
      Num_Features        : Tree.Index_Range := 1;
      Num_Outputs         : Tree.Index_Range := 1;
      Decision_Tree       : Tree.Tree_Class;
   end record;

   --  class DecisionTreeClassifier
   type Classifier (Split_Type, Leaf_Type, Feature_Type : Tree.Data_Type) is record
      Parameters : Base_Parameter_Data (Split_Type, Leaf_Type, Feature_Type);
      Attributes : Attribute_Data;
   end record;

   Value_Error : Exception;

   --  BaseDecisionTree.Fit (super of class DecisionTreeClassifier)
   --  Fit builds a decision tree classifier from the training set (X, Y).
   --  X : training input samples; a (n_samples, n_features) matrix
   --  Y : values (class labels); a (n_samples, n_outputs) matrix
   procedure Fit (aClassifier   : in out Classifier;
                  X             : ML_Types.List_Of_Value_Data_Lists;
                  Y             : in out ML_Types.List_Of_Value_Data_Lists;
                  Sample_Weight : in out Classifier_Types.Weight_List;
                  Check_Input   : Boolean := False);
   procedure Init (aClassifier    : in out Classifier;
                   Max_Leaf_Nodes : Integer := -1;
                   Random_State   : Integer := 0);
   function Predict (Self : in out Classifier;
                     X    : ML_Types.List_Of_Value_Data_Lists)
                     return ML_Types.Value_Data_List;

end Decision_Tree_Classifer;
