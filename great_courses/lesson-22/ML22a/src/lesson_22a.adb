
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  with Basic_Printing; use Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
--  with Neural_Utilities;
with Python; use Python;
--  with Python_22A;
with Python_API;
--  with Python_CLF;

with Support_22A; use Support_22A;

--  A clinical trial in the 1980s called the Infant Health and Development
--  Program or IHDP looked at the cognitive capacity of premature infants
--  and the impact of treatments on their development.
--  The IHDP dataset contains instances from their randomized study and
--  records properties of the children and their caregivers.
--  An important question the data can address is: does treatment from a
--  specialized therapist result in better outcomes than treatment from other
--  care givers?
procedure Lesson_22A is

   Project_Name           : constant String := "Lesson_22A ";
   Data_File              : constant String := "../../data/ihdp_npci_1.csv";
   Data                   : constant Data_Record := Get_Data (Data_File);
   X_String               : Unbounded_String ;
   Classifier             : Module;
   Model                  : Python_API.PyObject_Ptr;

   --  -------------------------------------------------------------------------
begin
   Put_Line (Project_Name);
--     Print_Data (Data, 1, 3);
   X_String := Get_X_Names (Data.Col_Names);
   Put_Line (Project_Name & "X_String set.");

   Python.Initialize;
   Classifier := Python.Import_File ("py_22a");
--     Model := Python_22A.Set_Model (Classifier, Data);
   Python_API.Py_DecRef (Model);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_22A;
