
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Directories; use Ada.Directories;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;
with NL_Types;

with Python;

with Aux_Utils;
with Word_Classification; use Word_Classification;

procedure Lesson_3_Aux2 is
   Routine_Name : constant String := "Lesson_3_Aux2 ";
   File_IE      : constant String := "ie.txt";
   File_EI      : constant String := "ei.txt";
   IE_Data      : constant ML_Types.Unbounded_List :=
                    Aux_Utils.Load_Data (File_IE);
   EI_Data      : constant ML_Types.Unbounded_List :=
                    Aux_Utils.Load_Data (File_EI);
   Labels       : ML_Types.Unbounded_List;
   Words        : ML_Types.Unbounded_List;
   Pronounce    : ML_Types.Unbounded_List;
   Data         : NL_Types.Boolean_List_2D;
   Classifier   : Python.Module;
begin
   New_Line;
   Put_Line ("Lesson 3 Aux2");
   Build_Dataset (IE_Data, EI_Data, Labels, Words, Pronounce, Data);

   --     Put_Line (Routine_Name & "Current_Directory: " & Current_Directory);
   Python.Initialize;
--     Python.Execute_String ("from sklearn import tree");
--     Python.Execute_String
--       ("clf = tree.DecisionTreeClassifier(max_leaf_nodes=8)");
   Classifier := Python.Import_File ("word_classifier_aux.py");
   New_Line;
   Put_Line (Routine_Name & "word_classifier_aux.py file executed");

   Put_Line ("----------------------------------------------");
   Put_Line (Routine_Name & "completed.");
   New_Line;

   Python.Finalize;

end Lesson_3_Aux2;
