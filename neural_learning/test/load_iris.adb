
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
--  with ML_Types;
with Printing;

package body Load_Iris is
--     use ML_Types;

   --  -------------------------------------------------------------------------

   procedure Load is
      use Ada.Containers;
      use Ada.Strings.Unbounded;
      use Classifier_Utilities;
      use Printing;
--        use Classifier_Types.Float_Package;
      Routine_Name    : constant String :=
                          "Classification.Classify_Iris ";
      Iris_Data       : constant Multi_Output_Data_Record :=
                            Load_Data ("src/iris.csv");
      Class_Names     : Class_Names_List;
      Features        : Feature_Names_List;
      Iris_Features   : constant Value_Data_Lists_2D :=
                          Iris_Data.Feature_Values;
      Num_Samples     : constant Natural := Natural (Iris_Features.Length);
      --  Iris_Target: num outputs x num classes
      Iris_Target     : Value_Data_Lists_2D;
   begin
      Put_Line (Routine_Name);
      Class_Names.Append (To_Unbounded_String ("Setosa"));
      Class_Names.Append (To_Unbounded_String ("Versicolour"));
      Class_Names.Append (To_Unbounded_String ("Virginica"));

      Assert (Num_Samples > 0, Routine_Name &
                " called with empty Features vector.");

      --  Iris_Target is 2D list num outputs x num classes
      Iris_Target := Iris_Data.Label_Values;
      Assert (Integer (Iris_Target.Length) = Num_Samples, Routine_Name &
                " invalid Iris_Target vector");

   end Load;

   --  -------------------------------------------------------------------------

end Load_Iris;
