
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
--  with Printing;

package body Load_Dataset is

    --  Target: num outputs x num classes
    procedure Load_Digits (Features, Target : out NL_Types.Value_Data_Lists_2D) is
        use Classifier_Utilities;
        Routine_Name   : constant String := "Load_Dataset.Load_Digits ";
        Digits_Data    : constant NL_Types.Multi_Output_Data_Record :=
                           Load_Data ("digits.csv");
        Class_Names    : NL_Types.Class_Names_List;
--          Feature_Names       : NL_Types.Feature_Names_List;
        Digit_Features : constant NL_Types.Value_Data_Lists_2D :=
                          Digits_Data.Feature_Values;
        Num_Samples    : constant Natural := Natural (Digit_Features.Length);
    begin
        Class_Names.Append (To_Unbounded_String ("0"));
        Class_Names.Append (To_Unbounded_String ("1"));
        Class_Names.Append (To_Unbounded_String ("2"));
        Class_Names.Append (To_Unbounded_String ("3"));
        Class_Names.Append (To_Unbounded_String ("4"));
        Class_Names.Append (To_Unbounded_String ("5"));
        Class_Names.Append (To_Unbounded_String ("6"));
        Class_Names.Append (To_Unbounded_String ("7"));
        Class_Names.Append (To_Unbounded_String ("8"));
        Class_Names.Append (To_Unbounded_String ("9"));
        Class_Names.Append (To_Unbounded_String ("10"));

        Assert (Num_Samples > 0, Routine_Name &
                  " called with empty Features vector.");

        --  Digits_Target is 2D list num outputs x num classes
        Target := Digits_Data.Label_Values;
        Assert (Integer (Target.Length) = Num_Samples, Routine_Name &
                  " invalid Digits Target vector");
        Features := Digits_Data.Feature_Values;

    end Load_Digits;

    --  -------------------------------------------------------------------------

    procedure Load_Iris (Features, Target : out NL_Types.Value_Data_Lists_2D) is
        use Classifier_Utilities;
        Routine_Name  : constant String := "Load_Dataset.Load_Iris ";
        Iris_Data     : constant NL_Types.Multi_Output_Data_Record :=
                          Load_Data ("iris.csv");
        Class_Names   : NL_Types.Class_Names_List;
        --        Features        : NL_Types.Feature_Names_List;
        Iris_Features : constant NL_Types.Value_Data_Lists_2D :=
                          Iris_Data.Feature_Values;
        Num_Samples   : constant Natural := Natural (Iris_Features.Length);
    begin
        Class_Names.Append (To_Unbounded_String ("Setosa"));
        Class_Names.Append (To_Unbounded_String ("Versicolour"));
        Class_Names.Append (To_Unbounded_String ("Virginica"));

        Assert (Num_Samples > 0, Routine_Name &
                  " called with empty Features vector.");
        Features := Iris_Data.Feature_Values;
        Target := Iris_Data.Label_Values;
        Assert (Integer (Target.Length) = Num_Samples, Routine_Name &
                  " invalid Iris Target vector");

    end Load_Iris;

    --  -------------------------------------------------------------------------

end Load_Dataset;
