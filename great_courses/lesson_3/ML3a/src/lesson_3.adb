
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Decision_Tree_Classification;
with ML_Types;
with TL_Utilities;
with Tree;
with Weights;

procedure Lesson_3 is
   use ML_Types;
   use TL_Utilities;
   Data_File : File_Type;
begin
   Put_Line ("Lesson 3 ");
   Open (Data_File, In_File, "../diabetes.csv");
   declare
      Row_Data    : Rows_Vector;
      Data        : Data_Record;
      Weight_Data : Weights.Weight_List;
   begin
      Put_Line ("Feature Names:");
      Load_CSV_Data (Data_File, Row_Data);
      Data := Split_Row_Data (Row_Data);
      declare
         aClassifier : Base_Decision_Tree.Classifier
           (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
      begin
         --  Fit function adjusts weights according to data values so that
         --  better accuracy can be achieved
         Decision_Tree_Classification.Classification_Fit
           (aClassifier, Data.Feature_Values, Data.Label_Values, Weight_Data);
      end;
   end;

   Close (Data_File);

end Lesson_3;
