
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
--  with Neural_Utilities;
with Python; use Python;
--  with Python_API;
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
   Data_File_Name         : constant String := "../../data/ihdp_npci_1.csv";
   Data                   : constant Data_Record := Get_Data (Data_File_Name);
   Classifier             : Module;
--     CLF                    : Python_API.PyObject_Ptr;

   --  -------------------------------------------------------------------------
begin
   Print_Data (Data);
   Python.Initialize;
   Classifier := Import_File ("lesson_22a");
--     Python_CLF.Call (Classifier, "show_tree", CLF, Words);
--     Python_API.Py_DecRef (CLF);
--     CLF := Python_CLF.Call (Classifier, "multinomial_fit",
--                             Train_Data.Features, Train_Data.Labels);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_22A;
