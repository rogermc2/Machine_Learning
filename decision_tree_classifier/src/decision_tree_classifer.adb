--  Based on scikit-learn/sklearn/tree/_classes.py
--  class DecisionTreeClassifier(ClassifierMixin, BaseDecisionTree)

--  with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;

package body Decision_Tree_Classifer is

   --  -------------------------------------------------------------------------
   --  L884
   procedure Classification_Fit
     (aClassifier    : in out Base_Decision_Tree.Classifier;
      X              : ML_Types.List_Of_Value_Data_Lists;
      Y              : ML_Types.List_Of_Value_Data_Lists;
      Y_Encoded      : out Classifier_Types.List_Of_Natural_Lists;
      Classes        : out ML_Types.List_Of_Value_Data_Lists;
      Sample_Weights : out Classifier_Types.Float_List) is
   begin
      --  L929
      Base_Decision_Tree.Base_Fit (aClassifier, X, Y, Y_Encoded, Classes,
                                   Sample_Weights);

   end Classification_Fit;

   --  -------------------------------------------------------------------------

   procedure Init (aClassifier    : in out Base_Decision_Tree.Classifier;
                   Max_Leaf_Nodes : Integer := -1;
                   Max_Depth      : Integer := -1;
                   Random_State   : Integer := 0) is
   begin
      Base_Decision_Tree.Init (aClassifier, Max_Leaf_Nodes, Max_Depth,
                               Random_State => Random_State);
   end Init;

   --  -------------------------------------------------------------------------
   --  L930 Predict_Probability predicts class probabilities of the
   --  input samples X.
   --  The predicted class probability is the fraction of samples of the same
   --  class in a leaf.
   function Predict_Probability (Self : in out Base_Decision_Tree.Classifier;
                                 X    : ML_Types.List_Of_Value_Data_Lists)
                                 return ML_Types.List_Of_Value_Data_Lists is
      use ML_Types;
      Proba      : Value_Data_List;
      All_Proba  : List_Of_Value_Data_Lists;
      Data       : Value_Record;
      Normalizer : Float := 0.0;
   begin
      --  L954
      Proba :=  Tree.Predict (Self.Attributes.Decision_Tree, X);
      for OP_Index in 1 .. Self.Attributes.Num_Outputs loop
         for index in Proba.First_Index .. Proba.Last_Index loop
            Data := Proba.Element (index);
            case Data.Value_Kind is
               when Float_Type =>
                  Normalizer := Normalizer + Data.Float_Value;
               when Integer_Type =>
                  Normalizer := Normalizer + Float (Data.Integer_Value);
               when others => null;
            end case;
         end loop;
         if Normalizer <= 0.0 then
            Normalizer := 1.0;
         end if;

         for index in Proba.First_Index .. Proba.Last_Index loop
            Data := Proba.Element (index);
            case Data.Value_Kind is
               when Float_Type =>
                  Data.Float_Value := Data.Float_Value / Normalizer;
               when Integer_Type =>
                  Data.Integer_Value :=
                    Integer (Float (Data.Integer_Value) / Normalizer);
               when others => null;
            end case;
            Proba.Replace_Element (index, Data);
         end loop;
         All_Proba.Append (Proba);

      end loop;

      return All_Proba;

   end Predict_Probability;

   --  -------------------------------------------------------------------------

end Decision_Tree_Classifer;
