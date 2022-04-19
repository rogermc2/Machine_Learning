
with Ada.Containers;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Mix;
with Base_Neural;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
--  with Printing;

with Support_4;

procedure Lesson_4A_Neuron is
   use Ada.Containers;
   use Support_4;
   use Multilayer_Perceptron;
   Routine_Name    : constant String := "Lesson_4A_Neuron ";
   Dataset_Name    : constant String := "mnist_784";
   Test_Size       : constant Positive := 1000;
   Train_Size      : constant Positive := 5000;
   --     Bunch           : Openml_Ada.Bunch_Data;
   aClassifier     : Multilayer_Perceptron.MLP_Classifier;
begin
   Put_Line (Routine_Name);
   declare
      Data    : constant Base_State :=
                  Get_State (Dataset_Name, Train_Size, Test_Size);
      Train_X : constant Float_Matrix := Data.Train_X;
      Train_Y : constant Integer_Matrix := Data.Train_Y;
      Test_X  : constant Float_Matrix := Data.Test_X;
      Test_Y  : constant Integer_Matrix := Data.Test_Y;
   begin
      --     if not Get_Classifier (Dataset_Name, aClassifier) then
      --        Printing.Print_Strings ("Features", Bunch.Feature_Names);

      Put_Line ("Train X length: " & Count_Type'Image (Train_X'Length));
      Put_Line ("Train Y length: " & Count_Type'Image (Train_Y'Length));
      Put_Line ("Test X length: " & Count_Type'Image (Test_X'Length));
      Put_Line ("Test Y length: " & Count_Type'Image (Test_Y'Length));

      aClassifier := C_Init (Max_Iter => 10000,
                             Activation => Base_Neural.Identity_Activation);

      --  Fit function adjusts weights according to data values so that better
      --  accuracy can be achieved
      Put_Line ("Neural_Fit");
      Fit (aClassifier, Train_X, Train_Y);
      Support_4.Save_Classifier (Dataset_Name, aClassifier);
       Put_Line ("Score" & Float'Image (aClassifier.Parameters.Validation_Scores.));
   end;  --  declare

   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_4A_Neuron;
