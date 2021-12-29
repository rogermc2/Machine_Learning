
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with ML_Types;

with Base_Decision_Tree;
with Classifier_Types;
with Criterion;
with Decision_Tree_Classification;
with Tree;
with Weights;

procedure Lesson_3 is
   use Utilities;
   use ML_Types;
   use Decision_Tree_Classification;

   Data_File    : File_Type;
begin
   Put_Line ("Lesson 3");
   Open (Data_File, In_File, "src/diabetes.csv");
   declare
      Row_Data      : Rows_Vector;
      Data          : Data_Record;
      Empty_Weights : Weights.Weight_List :=
                          Classifier_Types.Float_Package.Empty_Vector;
   begin
      Put_Line ("Feature Names:");
      Utilities.Load_CSV_Data (Data_File, Row_Data);
      Data := Split_Row_Data (Row_Data);
      declare
         aClassifier : Base_Decision_Tree.Classifier
           (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
      begin
         --  Fit function adjusts weights according to data values so that
         --  better accuracy can be achieved
         C_Init (aClassifier, "2", Criterion.Gini_Criteria);
--           Classification_Fit (aClassifier, Iris_Features, Iris_Target,
--                            No_Weights);
--           Estimate := Base_Decision_Tree.Fit
            Classification_Fit (aClassifier, Data.Feature_Values,
                                Data.Label_Values, Empty_Weights);
      end;
   end;

   Close (Data_File);

end Lesson_3;
