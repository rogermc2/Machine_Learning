
with Ada.Containers;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Base;
with Base_Neural;
--  with Load_Dataset;
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
   --     Test_Size       : constant Positive := 300;
   --     Train_Size      : constant Positive := 1400;
   aClassifier     : Multilayer_Perceptron.MLP_Classifier;
begin
   Put_Line (Routine_Name);
   declare
      --  Digits_Data   : constant Load_Dataset.Digits_Data_Record :=
      --                    Load_Dataset.Load_Digits ("../../Datasets/digits.csv");
      --        Features      : constant Real_Float_Matrix :=
      --                          To_Real_Float_Matrix (Digits_Data.Features);
      --        Target        : NL_Types.Integer_List;
      Data          : constant Base_State :=
                        Get_State (Dataset_Name, Train_Size, Test_Size);
      --  Data          : constant Base_State :=
      --                    Get_State (Digits_Data, Train_Size, Test_Size);
      Train_X       : constant Real_Float_Matrix := Data.Train_X;
      Train_Y       : constant Integer_Matrix := Data.Train_Y;
      Test_X        : constant Real_Float_Matrix := Data.Test_X;
      Test_Y        : constant Integer_Matrix := Data.Test_Y;
      Num_Classes   : constant Positive := 10;
      Train_Y_Bin   : Boolean_Matrix (Train_Y'Range, 1 .. Num_Classes);
      --        Test_Y_Bin    : Boolean_Matrix (Test_Y'Range, Test_Y'Range (2));
      Sample_Weight : Real_Float_Vector (1 .. 0);
   begin
      Put_Line ("Train X length: " & Count_Type'Image (Train_X'Length) & " x" &
                  Count_Type'Image (Train_X'Length (2)));
      Put_Line ("Train Y length: " & Count_Type'Image (Train_Y'Length) & " x" &
                  Count_Type'Image (Train_Y'Length (2)));
      Put_Line ("Test X length: " & Count_Type'Image (Test_X'Length));
      Put_Line ("Test Y length: " & Count_Type'Image (Test_Y'Length));

      --        aClassifier := C_Init (Max_Iter => 10000,
      aClassifier := C_Init (Max_Iter => 2000,
                             Activation => Base_Neural.Identity_Activation,
                             Verbose => False);

      --  Fit function adjusts weights according to data values so that better
      --  accuracy can be achieved
      Fit (aClassifier, Train_X, Train_Y, Train_Y_Bin);
      --        Test_Y_Bin := Multilayer_Perceptron.Validate_Input
      --          (aClassifier, Test_Y, Incremental => False);
      Put_Line ("Score: " & Float'Image (Base.Score
                (Self => aClassifier, X => Test_X, Y => Test_Y,
                 Sample_Weight => Sample_Weight)));
   end;  --  declare

   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_4A_Neuron;
