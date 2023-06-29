
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
with Python;
--  with Python_API;
--  with Python_CLF;

--  with Python_21A;
--  with Support_21A; use Support_21A;

procedure Lesson_21A is
   Program_Name         : constant String := "Lesson 21A ";
   Num_Rows             : constant Positive := 5;
   Num_Cols             : constant Positive := 10;
   Grid_Map             : constant Integer_Matrix (1 .. Num_Rows,
                                                   1 .. Num_Cols) :=
                            ((0,0,0,0,0,2,0,0,1,0),
                             (0,1,0,0,0,2,0,0,0,0),
                             (0,0,0,0,0,2,1,3,0,0),
                             (0,0,0,1,0,2,0,3,0,0),
                             (0,0,0,0,0,2,0,3,0,4));
   Classifier           : Python.Module;
   --     Tokenizer            : Python_API.PyObject_Ptr;
begin
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_21a");
   Python.Call (Classifier, "plot", Grid_Map);
--     Python.Call (Classifier, "plot_matrix", Grid_Map);
--     Python_API.Py_DecRef (Tokenizer);   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Lesson_21A;
