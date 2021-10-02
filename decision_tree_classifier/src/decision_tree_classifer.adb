--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

--  with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;

package body Decision_Tree_Classifer is

    --  -------------------------------------------------------------------------
    --  L884
    procedure Classification_Fit
      (aClassifier   : in out Base_Decision_Tree.Classifier;
       X             : ML_Types.List_Of_Value_Data_Lists;
       Y             : in out ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : out Classifier_Types.Float_List) is
    begin
        --  L929
        Base_Decision_Tree.Base_Fit  (aClassifier, X, Y, Sample_Weight);

    end Classification_Fit;

    --  -------------------------------------------------------------------------

    procedure Init (aClassifier    : in out Base_Decision_Tree.Classifier;
                    Max_Leaf_Nodes : Integer := -1;
                    Random_State   : Integer := 0) is
    begin
        Base_Decision_Tree.Init (aClassifier, Max_Leaf_Nodes, Random_State);
    end Init;

    --  -------------------------------------------------------------------------
    --  L930
    procedure Predict_Probability (Self : in out Base_Decision_Tree.Classifier;
                                   X    : ML_Types.List_Of_Value_Data_Lists) is
        Proba     : ML_Types.Value_Data_List;
        All_Proba : ML_Types.Value_Data_List;
    begin
        Proba :=  Tree.Predict (Self.Attributes.Decision_Tree, X);
    end Predict_Probability;

    --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
