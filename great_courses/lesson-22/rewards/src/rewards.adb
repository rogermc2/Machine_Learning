--  Based on:
--  https://github.com/py-why/dowhy/blob/main/docs/source/example_notebooks/dowhy_example_effect_of_memberrewards_program.ipynb

--  Suppose that a website has a membership rewards program where customers
--  receive additional benefits if they sign up.
--  How do we know if the program is effective?
--  Here the relevant causal question is:
--  What is the impact of offering membership rewards on total sales?
--  And the equivalent counterfactual question is,
--  If the current members had not signed up for the program, how much less
--  would they have spent on the website?
--  In formal language, we are interested in finding the Average Treatment
--   Effect on the Treated (ATT).
with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Strings;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
--  with Neural_Utilities;
with Python; use Python;
--  with Python_Rewards;
with Python_API;
--  with Python_CLF;

--  with Support_Rewards; use Support_Rewards;

procedure Rewards is
   Project_Name           : constant String := "Membership Rewards ";
   Num_Users              : constant Positive := 10000;
   Num_Months             : constant Positive := 12;
   Signup_Months          : Integer_Array (1 .. Num_Users);
--     Data                   : Data_Record;
--     X_String               : Unbounded_String;
   Classifier             : Module;
   Data_Frame             : Python_API.PyObject_Ptr;

   --  -------------------------------------------------------------------------
begin
   Put_Line (Project_Name);
   for index in Signup_Months'Range loop
      Signup_Months (index) := Maths.Random_Integer (1, Num_Months);
   end loop;
--     X_String := Get_X_Names (Data.Col_Names);

   Python.Initialize;
   Classifier := Python.Import_File ("rewards");
--     Model := Python_Rewards.Set_Model (Classifier, Data, X_String);
   Data_Frame := Python.Call (Classifier, "init_data", Num_Users, Num_Months,
                              Signup_Months);
   Python_API.Py_DecRef (Data_Frame);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Rewards;
