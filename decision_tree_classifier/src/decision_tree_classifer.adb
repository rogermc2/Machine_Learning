--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

--  with Ada.Text_IO; use Ada.Text_IO;

with Ada_Tree_Build;
with Base_Decision_Tree;
with Classifier_Types;

package body Decision_Tree_Classifer is
   use Base_Decision_Tree;

   --  -------------------------------------------------------------------------
   --  L884
   procedure Classification_Fit
     (aClassifier   : in out Base_Decision_Tree.Classifier;
      X             : ML_Types.List_Of_Value_Data_Lists;
      Y             : in out ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : out Classifier_Types.Float_List) is
      use Base_Decision_Tree;
   begin
      --  L929
      Base_Fit  (aClassifier, X, Y, Sample_Weight);

      --  L350
      Ada_Tree_Build.Build_Tree (aClassifier.Attributes.Decision_Tree,
                                 X, Y, Sample_Weight);
      --  L410
   end Classification_Fit;

   --  -------------------------------------------------------------------------

   procedure Init (aClassifier    : in out Classifier;
                   Max_Leaf_Nodes : Integer := -1;
                   Random_State   : Integer := 0) is
   begin
      aClassifier.Parameters.Random_State := Random_State;
      aClassifier.Parameters.Max_Leaf_Nodes := Max_Leaf_Nodes;
   end Init;

   --  -------------------------------------------------------------------------
   --  Based on class.py fit L431 Predict
   --  Prune tree using Minimal Cost-Complexity Pruning.
   function Predict (Self : in out Classifier;
                     X    : ML_Types.List_Of_Value_Data_Lists)
                      return ML_Types.Value_Data_List is
   begin
      return Tree.Predict (Self.Attributes.Decision_Tree, X);
   end Predict;

   --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
