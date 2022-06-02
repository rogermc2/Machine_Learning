
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
--  with Label;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Stochastic_Optimizers;

--  Test_Gradient makes sure that the activation functions and their
--  derivatives are correct
procedure Test_Gradient is
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;

   Routine_Name  : constant String := "Test_Gradient ";
   Num_Samples   : constant := 5;
   Num_Features  : constant := 10;
   X             : Real_Float_Matrix (1 .. Num_Samples, 1 .. Num_Features);
   Y             : Integer_Array (1 .. Num_Samples);
   Y2            : Integer_Matrix (1 .. Num_Samples, 1 .. 1);
   Layer_Sizes   : NL_Types.Integer_List;
   aClassifier   : MLP_Classifier;
   Params        : Parameters_List;
   Coeff1_Test   : constant Real_Float_Matrix (1 .. 3, 1 .. 2) :=
                     ((0.098, 0.195756), (0.2956664, 0.096008),
                      (0.4939998, -0.002244));
   Coeff2_Test   : constant Real_Float_Matrix (1 .. 2, 1 .. 1) :=
                     ((1 => 0.04706), (1 => 0.154089));
   Coeff1_Error  : Real_Float_Matrix
     (Coeff1_Test'Range, Coeff1_Test'Range (2));
   Coeff2_Error  : Real_Float_Matrix
     (Coeff2_Test'Range, Coeff2_Test'Range (2));
--     LB            : Label.Label_Binarizer;

--     procedure Set_Weights (Self : in out MLP_Classifier) is
--        Coeffs_1     : constant Real_Float_Matrix (1 .. 3, 1 .. 2) :=
--                         ((0.1, 0.2), (0.3, 0.1), (0.5, 0.0));
--        Intercepts_1 : constant Real_Float_Vector (1 .. 2) := (0.1, 0.1);
--        Coeffs_2     : constant Real_Float_Matrix (1 .. 2, 1 .. 1) :=
--                         ((1 => 0.1), (1 => 0.2));
--        Intercepts_2 : constant Real_Float_Vector (1 .. 1) := (1 => 1.0);
--        Params_1     : Parameters_Record (3, 2);
--        Params_2     : Parameters_Record (2, 1);
--     begin
--        Params_1.Coeff_Gradients := Coeffs_1;
--        Params_1.Intercept_Grads := Intercepts_1;
--        Params_2.Coeff_Gradients := Coeffs_2;
--        Params_2.Intercept_Grads := Intercepts_2;
--        Self.Attributes.Params.Append (Params_1);
--        Self.Attributes.Params.Append (Params_2);
--
--     end Set_Weights;

begin
   Put_Line (Routine_Name);
   Layer_Sizes.Append (10);
   for num_labels in 2 .. 3 loop
      for sample in X'Range loop
         for feature in X'Range (2) loop
            X (sample, feature) := abs (Maths.Random_Float);
         end loop;
      end loop;

      for value in Y'Range loop
         Y (value) := value mod num_labels + 1;
         Y2 (value, 1) := Y (value);
      end loop;

      declare
--           Y_Bin : Boolean_Matrix := Label.Fit_Transform (LB, Y);
      begin
         for activ_type in Base_Neural.Activation_Type'Range loop
            aClassifier := C_Init
              (Activation => activ_type,
               Hidden_Layer_Sizes => Layer_Sizes,
               Solver => Stochastic_Optimizers.Lbfgs_Solver,
               Alpha => 10.0 ** (-5), Learning_Rate_Init => 0.2,
               Random_State => 1, Max_Iter => 1);

            Init_Optimizer (aClassifier);
            Fit (aClassifier, X, Y2);

            Params := aClassifier.Attributes.Params;
            Coeff1_Error :=
              abs (Coeff1_Test - Params.Element (1).Coeff_Gradients);
            Printing.Print_Float_Matrix_Formated ("Coeffs (1) errors", Coeff1_Error, 3);
            Coeff2_Error :=
              abs (Coeff2_Test - Params.Element (2).Coeff_Gradients);
            Printing.Print_Float_Matrix_Formated ("Coeffs (2) errors", Coeff2_Error, 3);
         end loop;
      end;
      --      Assert (Coeff1_Test = Params.Element (1).Coeff_Gradients,
      --              "Coeffs (1) Test failed");
      --      Assert (Coeff2_Test = Params.Element (2).Coeff_Gradients,
      --              "Coeffs (2) Test failed");
      --      Put_Line ("Coeffs tests passed");
   end loop;

end Test_Gradient;
