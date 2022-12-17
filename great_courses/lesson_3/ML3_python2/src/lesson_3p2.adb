
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types; use ML_Types;

with Classifier_Utilities;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Tree_Printing;

procedure Lesson_3P2 is
   use ML_Types.String_Package;
   Routine_Name  : constant String := "Lesson_3P2 ";
   Data          : constant Multi_Output_Data_Record :=
                     Classifier_Utilities.Load_Data ("../diabetes.csv");
   --  feats
   Feature_Names : constant String_List := Data.Feature_Names;
   --  dat
   X_Data_List   : constant Value_Data_Lists_2D := Data.Feature_Values;
   --  labs
   Labels_List   : constant Value_Data_Lists_2D := Data.Label_Values;
   Num_Samples   : constant Natural := Natural (X_Data_List.Length);
   X_Data        : constant Integer_Matrix := To_Integer_Matrix (X_Data_List);
   Labels        : constant Integer_Matrix := To_Integer_Matrix (Labels_List);
   Names_Cursor  : String_Package.Cursor := Feature_Names.First;
   Features      : ML_Types.Unbounded_List;
   Classifier    : Python.Module;
begin
   Put_Line (Routine_Name);
   Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");
   Put_Line (Routine_Name & "Num_Samples:" & Integer'Image (Num_Samples));
   while Has_Element (Names_Cursor) loop
      Features.Append (Element (Names_Cursor));
      Next (Names_Cursor);
   end loop;

--     Printing.Print_Unbounded_List ("Features", Features);
   Tree_Printing.Print_Value_Data_List ("Features row 16", X_Data_List.Element (16));
   New_Line;

   Python.Initialize;

   Classifier := Python.Import_File ("lesson_3p2");
   Python.Call (Classifier, "classify", X_Data, Labels, Features);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_3P2;