
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Types; use NL_Types;

with Base_Neural;
with Data_Splitter;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Openml_Ada;
--  with Printing;
with Utilities;

with Support_4;

procedure Lesson_4A_Neuron is
    use Ada.Containers;
    use Support_4;
    use Multilayer_Perceptron;
    Routine_Name    : constant String := "Lesson_4A_Neuron ";
    Dataset_Name    : constant String := "mnist_784";
    Test_Size       : constant Positive := 1000;
    Train_Size      : constant Positive := 5000;
    Bunch           : Openml_Ada.Bunch_Data;
    X               : Float_Matrix (1 .. Test_Size + Train_Size,
                                    1 .. Test_Size + Train_Size);  --  rows of columns of values
                                      --  samples x features
    Y               : Integer_List;
    aClassifier     : Multilayer_Perceptron.MLP_Classifier;
--      Num_Samples     : Positive;
    Train_X         : Float_Matrix (1 .. Train_Size, 1 .. Train_Size);
    Train_Y         : Integer_Array (1 .. Train_Size);
    Test_X          : Float_Matrix (1 .. Test_Size, 1 .. Test_Size);
    Test_Y          : Integer_Array (1 .. Test_Size);
begin
    Put_Line (Routine_Name);
    if not Get_State (Dataset_Name, Train_X, Train_Y, Test_X, Test_Y, Bunch)
    then
      declare
         X : Float_Matrix := To_Float_Matrix (Bunch.Data);
         Y : Integer_Array := To_Integer_Array (Bunch.Target);
      begin
--          Num_Samples := Positive (X.Length);

        Put_Line (Routine_Name & "Num_Samples" & Integer'Image (Positive (X'Length)));
        Assert (X'Length > 0, Routine_Name & "X is empty.");

        Assert (Natural (Y'Length) = Positive (X'Length), Routine_Name &
                  "Y length" & Count_Type'Image (Y'Length) &
                  " is different to X length" & Natural'Image (Positive (X'Length)));
        --        Printing.Print_Float_List ("Features row 16", X.Element (16));

        Put_Line (Routine_Name & "permuting");
        X := Utilities.Permute (X);
        Put_Line (Routine_Name & "X permuted");
        Utilities.Permute (Y);
        Put_Line (Routine_Name & "Y permuted");
        --        Printing.Print_Float_List ("permuted features row 16", X.Element (16));
        Put_Line (Routine_Name & "splitting data");
        Data_Splitter.Train_Test_Split (X, Y, Test_Size, Train_Size,
                                        Test_X, Test_Y, Train_X, Train_Y);
        Put_Line ("Requested train size: " & Integer'Image (Train_Size));
        Put_Line ("Train data length: " & Count_Type'Image (Train_X'Length));
        X.Clear;
        Y.Clear;
        Save_State (Dataset_Name, Train_X, Train_Y, Test_X, Test_Y,
                    Bunch);
        end;  --  declare
    end if;

    --     if not Get_Classifier (Dataset_Name, aClassifier) then
    --        Printing.Print_Strings ("Features", Bunch.Feature_Names);

    Put_Line ("Train data length: " & Count_Type'Image (Train_X.Length));
    Put_Line ("Train Y length: " & Count_Type'Image (Train_Y.Length));
    Put_Line ("Test data length: " & Count_Type'Image (Test_X.Length));

--      aClassifier := C_Init (Max_Iter => 10000,
    aClassifier := C_Init (Max_Iter => 4,
                           Activation => Base_Neural.Identity_Activation);

    --  Fit function adjusts weights according to data values so that better
    --  accuracy can be achieved
    Put_Line ("Neural_Fit");
    Fit (aClassifier, Train_X, Train_Y);
    Support_4.Save_Classifier (Dataset_Name, aClassifier);

    Put_Line ("----------------------------------------------");
    New_Line;

end Lesson_4A_Neuron;
