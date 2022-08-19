
--  Based on scikit-learn/sklearn/neural_network/tests/test_mlp.py test_gradient
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Label;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Test_Support;
with Stochastic_Optimizers;

with Loss_Functions; use Loss_Functions;

--  Test_Gradient makes sure that the activation functions and their
--  derivatives are correct
procedure Test_Gradient is
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;

   Routine_Name       : constant String := "Test_Gradient ";
   Num_Samples        : constant Positive := 5;
   Num_Features       : constant Positive := 10;
   Hidden_Layer_Sizes : constant Positive := 10;
   X                  : Real_Float_Matrix (1 .. Num_Samples, 1 .. Num_Features);
   Y                  : Integer_Matrix (1 .. Num_Samples, 1 .. 1);
   LB                 : Label.Label_Binarizer;
   Layer_Sizes        : NL_Types.Integer_List;
   aClassifier        : MLP_Classifier;
   Layer_Units        : NL_Types.Integer_List;
   Fan_In             : Positive;
   Fan_Out            : Positive;
   Params             : Parameters_List;
   Theta              : Parameters_List;
begin
   Put_Line (Routine_Name);
   Layer_Sizes.Append (Hidden_Layer_Sizes);
   for sample in X'Range loop
      for feature in X'Range (2) loop
         X (sample, feature) := abs (Maths.Random_Float);
      end loop;
   end loop;

   Layer_Units.Append (Num_Features);
   Layer_Units.Append (Hidden_Layer_Sizes);

   for num_labels in 2 .. 2 loop
      for value in Y'Range loop
         Y (value, 1) := value mod num_labels + 1;
      end loop;

      declare
         --  L194
         Y_Bin : constant Binary_Matrix := Label.Fit_Transform (LB, Y);
      begin
         --  L196
--           for activ_type in Base_Neural.Activation_Type'Range loop
         for activ_type in Base_Neural.Activation_Type'First ..
            Base_Neural.Activation_Type'First loop
            New_Line;
            Put_Line (Routine_Name & "Activation Type: " &
                        Base_Neural.Activation_Type'Image (activ_type));
            aClassifier := C_Init
              (Activation => activ_type,
               Hidden_Layer_Sizes => Layer_Sizes,
               Solver => Stochastic_Optimizers.Lbfgs_Solver,
               Alpha => 10.0 ** (-5), Learning_Rate_Init => 0.2,
               Random_State => 1, Max_Iter => 1);
            Init_Optimizer (aClassifier);
            --  L206
            Fit (aClassifier, X, Y);
            Layer_Units.Append (aClassifier.Attributes.N_Outputs);

            --  L208
            --  aClassifier.Attributes.Params is set by
            --  Multilayer_Perceptron.Initialize on first pass of
            --  Multilayer_Perceptron.Fit
            Theta := aClassifier.Attributes.Params;

--              Put_Line (Routine_Name & "L208 Coeff_Gradients Length:" &
--                          Integer'Image (Positive (aClassifier.Attributes.Params.Element (1).Coeff_Gradients'Length)));
--              Put_Line (Routine_Name & "L208 Theta_Length:" &
--                          Integer'Image (Positive (Theta.Length)));
            --              Put_Line (Routine_Name &
            --                          "L208 Theta (1).Coeff_Gradients size" &
            --                          Integer'Image (Theta (1).Coeff_Gradients'Length) & " x"
            --                          & Integer'Image (Theta (1).Coeff_Gradients'Length (2)));
            --              Put_Line (Routine_Name &
            --                          "L208 Theta.Intercept_Grads (1) length" &
            --                          Integer'Image (Theta (1).Intercept_Grads'Length));
            --              Put_Line (Routine_Name &
            --                          "L208 Theta (2).Coeff_Gradients length" &
            --                          Integer'Image (Theta (2).Coeff_Gradients'Length) & " x"
            --                          & Integer'Image (Theta (2).Coeff_Gradients'Length (2)));
            --              Put_Line (Routine_Name &
            --                          "L208 Theta.Intercept_Grads (2) length" &
            --                          Integer'Image (Theta (2).Intercept_Grads'Length));

            --  L212  Initialize
            Params.Clear;
            for layer in 1 .. aClassifier.Attributes.N_Layers - 1 loop
               --                 Put_Line (Routine_Name & "L222 layer" & Integer'Image (layer));
               Fan_In := Layer_Units (layer);
               Fan_Out := Layer_Units (layer + 1);

               --  L224
               declare
                  Param_Rec : Parameters_Record (Fan_In, Fan_Out);
               begin
                  Param_Rec.Coeff_Gradients := Zero_Matrix (Fan_In, Fan_Out);
                  Param_Rec.Intercept_Grads := Zero_Array (Fan_Out);
                  Params.Append (Param_Rec);
               end;
            end loop;

            --  L226
            declare
               --  N = Theta_Length
               Theta_Length : constant Positive := Positive (Theta.Length);
               Num_Grad     : Real_Float_Vector (1 .. Theta_Length);
               Loss_Grad    : Loss_Grad_Result;
            begin
--                 Put_Line (Routine_Name & "Theta_Length:" & Integer'Image (Theta_Length));
--                 New_Line;
--                 Put_Line (Routine_Name & "L239 analytically compute the gradients");
               --  L233 analytically compute the gradients
               Loss_Grad := Loss_Grad_Function
                 (aClassifier, Theta, X, Y_Bin, Params);
               Put_Line (Routine_Name & "analytically computed loss" &
                           Float'Image (Loss_Grad.Loss));

               New_Line;
               Put_Line (Routine_Name &
                           "L239 numerically compute the gradients");
               --  L239 numerically compute the gradients
               Num_Grad := Numerical_Loss_Grad
                 (aClassifier, Theta, X, Y_Bin, Params);

               for index in Loss_Grad.Gradients.First_Index ..
                 Loss_Grad.Gradients.Last_Index loop
--                    Test_Support.Print_Parameters ("Loss_Grad.Gradients",
--                                               Loss_Grad.Gradients (index));
                  Test_Support.Print_Float_Array
                    ("Num_Grad (" & Integer'Image (index) & ")", Num_Grad);
               end loop;
            end;
         end loop;
      end;
--        Put_Line (Routine_Name & "outer loop last");
      New_Line;
   end loop;

end Test_Gradient;
