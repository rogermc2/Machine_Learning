
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with IL_Types; use IL_Types;

with Base;
with Data_Splitter;
with Multilayer_Perceptron;
with Openml_Ada;
with Printing;
with Utilities;

with Support_4;

procedure Lesson_4_Neuron is
   use Ada.Containers;
   use Support_4;
   use Multilayer_Perceptron;
   Routine_Name    : constant String := "Lesson_4_Neuron ";
   Dataset_Name    : constant String := "mnist_784";
   Test_Size       : constant Positive := 1000;
   Train_Size      : constant Positive := 5000;
   Bunch           : Openml_Ada.Bunch_Data;
   X               : Float_List_2D;  --  rows of columns of values
   Y               : Integer_List;
   --     X_Indices       : Integer_List;
   --     Y_Indices       : Integer_List;
   aClassifier     : Multilayer_Perceptron.MLP_Classifier;
   Num_Samples     : Positive;
   Train_X         : Float_List_2D;
   Train_Y         : Integer_List;
   Test_X          : Float_List_2D;
   Test_Y          : Integer_List;
   Prediction_List : Integer_List;
   Prediction      : Integer;
   Correct         : Natural := 0;
begin
   Put_Line (Routine_Name);
   if not Get_State (Dataset_Name, Train_X, Train_Y, Test_X, Test_Y, Bunch) then
      X := Bunch.Data;
      Y := Bunch.Target;
      Num_Samples := Positive (X.Length);

      Put_Line (Routine_Name & "Num_Samples" & Integer'Image (Num_Samples));
      Assert (X.Length > 0, Routine_Name & "X is empty.");

      Assert (Natural (Y.Length) = Num_Samples, Routine_Name &
                "Y length" & Count_Type'Image (Y.Length) &
                " is different to X length" & Natural'Image (Num_Samples));
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
      Put_Line ("Train data length: " & Count_Type'Image (Train_X.Length));
      X.Clear;
      Y.Clear;
      Save_State (Dataset_Name, Train_X, Train_Y, Test_X, Test_Y,
                  Bunch);
   end if;

--     if not Get_Classifier (Dataset_Name, aClassifier) then
--        Printing.Print_Strings ("Features", Bunch.Feature_Names);
      --        Printing.Print_Float_List ("Train features row 16", Train_X.Element (16));
      --        Printing.Print_Float_List ("Test features row 16", Test_X.Element (16));

      --        Printing.Print_Float_List ("Train features row 417",
      --                                           Train_X.Element (417));

      --        Put_Line (Routine_Name & "Plotting");
      --        Plotting.Display_Image (Train_X.Element (4));
      --        Plotting.Display_Image (Test_X.Element (4));

      aClassifier := C_Init (Max_Iter => 10000, Activation => Base.Identity_Activation);

      --  Fit function adjusts weights according to data values so that better
      --  accuracy can be achieved
      Put_Line ("Classification_Fit");
      Fit (aClassifier, Train_X, Train_Y);
--        Support_4.Save_Classifier (Dataset_Name, aClassifier);
--     end if;

   Put_Line ("----------------------------------------------");
   New_Line;

   Put_Line ("Train data length: " & Count_Type'Image (Train_X.Length));
   Put_Line ("Test data length: " & Count_Type'Image (Test_X.Length));
--     Prediction_List := Base_Decision_Tree.Predict (aClassifier, Train_X);
   for index in Train_X.First_Index .. Train_X.Last_Index loop
      --        Put_Line (Routine_Name & "Train_X index" & Integer'Image (index));
      Prediction := Prediction_List.Element (index);
      if Prediction = Train_Y.Element (index) then
         Correct := Correct + 1;
      end if;
   end loop;

   Put_Line ("Prediction: " &
               Float'Image (100.0 * Float (Correct) / Float (Train_X.Length)));
   New_Line;

end Lesson_4_Neuron;
