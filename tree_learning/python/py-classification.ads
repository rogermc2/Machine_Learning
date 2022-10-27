
with Base_Decision_Tree; use Base_Decision_Tree;
with Criterion;
with Tree;
with Weights;

package Py.Classification is

   procedure Create_Classifier
     (Name : in out Classifier; Min_Samples_Split : String :="";
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

end Py.Classification;
