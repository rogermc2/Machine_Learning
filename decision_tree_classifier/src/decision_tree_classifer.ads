
with Classifier_Types;
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

with Base_Decision_Tree;
with Classifier_Types;

package Decision_Tree_Classifer is
   --  Gini Impurity is a measurement of the likelihood of an incorrect
   --  classification of a new instance of a random variable.
--     type Classifier_Criteria_Type is (Gini_Criteria, Entropy_Criteria);
--     type Regressor_Criteria_Type is (MSE_Criteria, Friedman_MSE_Criteria, MAE_Criteria);
--     type Splitter_Type is (Best_Splitter, Random_Splitter);
--     Type State is (None);
--     Type Weight_Type is (None);

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

   Classifier_Error : Exception;
   Value_Error      : Exception;

    procedure Classification_Fit
      (aClassifier    : in out Base_Decision_Tree.Classifier;
       X              : ML_Types.List_Of_Value_Data_Lists;
       Y              : ML_Types.List_Of_Value_Data_Lists;
       Y_Encoded      : out Classifier_Types.List_Of_Natural_Lists;
       Classes        : out ML_Types.List_Of_Value_Data_Lists;
       Sample_Weights : out Classifier_Types.Float_List);
   procedure Init (aClassifier    : in out Base_Decision_Tree.Classifier;
                   Max_Leaf_Nodes : Integer := -1;
                   Random_State   : Integer := 0);
   function Predict_Probability (Self : in out Base_Decision_Tree.Classifier;
                                  X    : ML_Types.List_Of_Value_Data_Lists)
                                 return ML_Types.List_Of_Value_Data_Lists;

end Decision_Tree_Classifer;
