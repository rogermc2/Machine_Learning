
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Base;
with Base_Neural;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Printing;
with Support_4;

procedure Lesson_4B_Neuron is
   use Ada.Containers;
   use Support_4;
   use Multilayer_Perceptron;
   Routine_Name       : constant String := "Lesson_4B_Neuron ";
   Dataset_Name       : constant String := "mnist_784";
   Test_Size          : constant Positive := 1000;
   Train_Size         : constant Positive := 5000;
   aClassifier        : Multilayer_Perceptron.MLP_Classifier;
   Hidden_Layer_Sizes : NL_Types.Integer_List;
begin
   Put_Line (Routine_Name);
   declare
      Data          : constant Base_State :=
                        Get_State (Dataset_Name, Train_Size, Test_Size);
      Train_X       : constant Real_Float_Matrix := Data.Train_X;
      Train_Y       : constant Integer_Matrix := Data.Train_Y;
      Test_X        : constant Real_Float_Matrix := Data.Test_X;
      Test_Y        : constant Integer_Matrix := Data.Test_Y;
   begin
      Put_Line ("Train X length: " & Count_Type'Image (Train_X'Length) & " x" &
                  Count_Type'Image (Train_X'Length (2)));
      Put_Line ("Test X length: " & Count_Type'Image (Test_X'Length));
      New_Line;

      for index in 1 .. 2 loop
         Hidden_Layer_Sizes.Clear;
         Hidden_Layer_Sizes.Append (index * 100);
         aClassifier := C_Init (Max_Iter => 1000,
                                Hidden_Layer_Sizes => Hidden_Layer_Sizes,
                                Activation => Base_Neural.Identity_Activation,
                                Verbose => False);
         --  The Fit function adjusts weights according to data values so
         --  that better accuracy can be achieved
         Fit (aClassifier, Train_X, Train_Y);
         --  Score uses Predict to compare predicted Y values, based on Test_X,
         --  with actual Test_Y values

         Put_Line
           (Integer'Image (index * 10) & " Score: " &
              Float'Image (Base.Score (Self => aClassifier,
                                       X => Test_X, Y => Test_Y)));
      end loop;
   end;  --  declare

   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_4B_Neuron;
