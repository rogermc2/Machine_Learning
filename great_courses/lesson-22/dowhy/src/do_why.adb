
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  with Basic_Printing; use Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
--  with Neural_Utilities;
with Python; use Python;
with Python_Do_Why;
with Python_API;
--  with Python_CLF;

with Support_Do_Why; use Support_Do_Why;

procedure Do_Why is

   Project_Name           : constant String := "Do Why ";
   Data_File              : constant String := "../../data/ihdp_npci_1.csv";
   Data                   : constant Data_Record := Get_Data (Data_File);
   X_String               : Unbounded_String;
   Classifier             : Module;
   Model                  : Python_API.PyObject_Ptr;

   --  -------------------------------------------------------------------------
begin
   Put_Line (Project_Name);
   X_String := Get_X_Names (Data.Col_Names);

   Python.Initialize;
   Classifier := Python.Import_File ("do_why");
   Model := Python_Do_Why.Set_Model (Classifier, Data, X_String);
   Call (Classifier, "identify_effect", Model);
   Python_API.Py_DecRef (Model);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Do_Why;
