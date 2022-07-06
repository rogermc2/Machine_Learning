
--  Based on scikit-learn/sklearn/neural_network/tests/test_mlp.py test_gradient
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Label;
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

   Routine_Name       : constant String := "Test_Gradient ";
   Eps                : constant Float := 10.0 ** (-5);
   Num_Samples        : constant Positive := 5;
   Num_Features       : constant Positive := 10;
   Hidden_Layer_Sizes : constant Positive := 10;
   X                  : Real_Float_Matrix (1 .. Num_Samples, 1 .. Num_Features);
   Y                  : Integer_Array (1 .. Num_Samples);
   Y2                 : Integer_Matrix (1 .. Num_Samples, 1 .. 1);
   LB                 : Label.Label_Binarizer;
   Layer_Sizes        : NL_Types.Integer_List;
   aClassifier        : MLP_Classifier;
   Activations        : Real_Matrix_List;
   Deltas             : Real_Matrix_List;
   Layer_Units        : NL_Types.Integer_List;
   Fan_In             : Positive;
   Fan_Out            : Positive;
   Params             : Parameters_List;
   Theta_List         : Real_Matrix_List;
   Theta              : Real_Vector_List;
   Loss               : Float;
   pragma Unreferenced (Loss);

   function Loss_Grad_Function
     (Self        : in out MLP_Classifier; Params : Parameters_List;
      X           : Real_Float_Matrix; Y         : Boolean_Matrix;
      Activations : Real_Matrix_List;
      Gradients   :  Stochastic_Optimizers.Parameters_List)
      return Loss_Grad_Result is
      Args : Loss_Grad_Args (X'Length, X'Length (2), Y'Length (2));
   begin
      Args.Self := Self;
      Args.Params := Params;
      Args.X := X;
      Args.Y := Y;
      Args.Activations := Activations;
      Args.Gradients := Gradients;
      Put_Line ("Loss_Grad_Function Args.X'Length" &
                  Integer'Image (Args.X'Length));
      Put_Line ("Args.Y'Length" & Integer'Image (Args.Y'Length));
      Put_Line ("Args.Params Length" &
                  Integer'Image (Integer (Args.Params.Length)));
      Put_Line ("Args.Activations Length" &
                  Integer'Image (Integer (Args.Activations.Length)));
      Put_Line ("Args.Gradients Length" &
                  Integer'Image (Integer (Args.Gradients.Length)));
      return Loss_Grad_LBFGS (Args);

   end Loss_Grad_Function;

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

   for num_labels in 2 .. 3 loop
      for value in Y'Range loop
         Y (value) := value mod num_labels + 1;
         Y2 (value, 1) := Y (value);
      end loop;

      --  L196
      for activ_type in Base_Neural.Activation_Type'Range loop
         aClassifier := C_Init
           (Activation => activ_type,
            Hidden_Layer_Sizes => Layer_Sizes,
            Solver => Stochastic_Optimizers.Lbfgs_Solver,
            Alpha => 10.0 ** (-5), Learning_Rate_Init => 0.2,
            Random_State => 1, Max_Iter => 1);
         Init_Optimizer (aClassifier);
         --  L206
         Fit (aClassifier, X, Y2);

         for index in aClassifier.Attributes.Params.First_Index ..
            aClassifier.Attributes.Params.Last_Index loop
                Theta_List.Append
                  (aClassifier.Attributes.Params.Element (index).Coeff_Gradients +
                  aClassifier.Attributes.Params.Element (index).Intercept_Grads);
         end loop;
         for index in Theta_List.First_Index ..Theta_List.Last_Index loop
                Theta.Append (Flatten (Theta_List (index)));
         end loop;

         Layer_Units.Append (aClassifier.Attributes.N_Outputs);

         Activations.Clear;
         Deltas.Clear;
         Params.Clear;
         --  L217  Initialize
         New_Line;
         Activations.Append (X);
         for layer in 1 .. aClassifier.Attributes.N_Layers - 1 loop
            Put_Line (Routine_Name & "L222 layer" & Integer'Image (layer));
            Fan_In := Layer_Units (layer);
            Fan_Out := Layer_Units (layer + 1);

            --  L224
            declare
               Param_Rec : Parameters_Record (Fan_In, Fan_Out);
            begin
               Params.Clear;
               Param_Rec.Coeff_Gradients := Zero_Matrix (Fan_In, Fan_Out);
               Param_Rec.Intercept_Grads := Zero_Array (Fan_Out);
               Params.Append (Param_Rec);
               Params := aClassifier.Attributes.Params;
            end;
         end loop;
         --  L226
         Put_Line (Routine_Name & "L226 end initialization loop");
         New_Line;
         declare
            Y_Bin        : constant Boolean_Matrix :=
                             Label.Fit_Transform (LB, Y);
            --  N = Theta_Length
            Theta_Length : constant Positive := Positive (Theta.Length);
            Eye          : constant Real_Float_Matrix :=
                             Unit_Matrix (Theta_Length);
            Num_Grad     : array (1 .. Theta_Length) of Loss_Grad_Result;
            dTheata      : Real_Vector (1 .. Theta_Length);
            Theta_P      : Parameters_List;
            Theta_M      : Parameters_List;
            Loss_Grad    : Loss_Grad_Result;
         begin
            --  L233 analytically compute the gradients
            Put_Line (Routine_Name & "L233");
            Loss_Grad := Loss_Grad_Function (aClassifier, Theta, X, Y_Bin,
                                             Activations, Params);

            --  L239 numerically compute the gradients

            Put_Line (Routine_Name & "L239");
            for index in 1 .. Positive (Theta.Length) loop
               --  L240
               for e_row in 1 .. Positive (Theta.Length) loop
                  dTheata (e_row) := Eye (e_row, index) * Eps;
               end loop;

               Put_Line (Routine_Name & "L241");
               declare
                  use Parameters_Package;
                  Theta_P  : Parameters_Record (Theta (index).Num_Rows,
                                                    Theta (index).Num_Cols);
                  Theta_M  : Parameters_Record (Theta (index).Num_Rows,
                                                    Theta (index).Num_Cols);
                  Loss_Grad_P  : Loss_Grad_Result;
                  Loss_Grad_M  : Loss_Grad_Result;
                  Grad_Diff    : Parameters_List;
               begin
                  for t_index in Theta.First_Index .. Theta.Last_Index loop
                     Theta_Rec_P := Theta (t_index);
                     Theta_Rec_M := Theta_Rec_P;
                     Put_Line (Routine_Name & "Theta_rec initialized");
                     Theta_Rec_P.Coeff_Gradients :=
                       Theta_Rec_P.Coeff_Gradients + dTheata;
                     Put_Line (Routine_Name & "Theta_Rec_P.Coeff_Gradients set");
                     Theta_Rec_P.Intercept_Grads :=
                       Theta_Rec_P.Intercept_Grads + dTheata;
                     Theta_P.Append (Theta_Rec_P);
                     Theta_Rec_M.Coeff_Gradients :=
                       Theta_Rec_M.Coeff_Gradients - dTheata;
                     Theta_Rec_M.Intercept_Grads :=
                       Theta_Rec_M.Intercept_Grads - dTheata;
                  end loop;
                  Put_Line (Routine_Name & "t_index loop done");

                  for index in Loss_Grad_P.Gradients.First_Index ..
                    Loss_Grad_P.Gradients.Last_Index loop
                     Grad_Diff.Append
                       ((Loss_Grad_P.Gradients (index) -
                          Loss_Grad_M.Gradients (index)) / (2.0 * Eps));
                  end loop;

                  Put_Line (Routine_Name & "242");
                  --  L242
                  Loss_Grad_P := Loss_Grad_Function
                    (Self => aClassifier, Params => Theta_P, X => X,
                     Y => Y_Bin, Activations => Activations,
                     Gradients => Params);
                  Loss_Grad_M := Loss_Grad_Function
                    (Self => aClassifier, Params => Theta_M, X => X,
                     Y => Y_Bin, Activations => Activations,
                     Gradients => Params);
                  Num_Grad (index).Loss :=
                    (Loss_Grad_P.Loss - Loss_Grad_M.Loss) / (2.0 * Eps);
                  Num_Grad (index).Gradients := Grad_Diff;
               end;
            end loop;

            for index in Params.First_Index .. Params.Last_Index loop
               Printing.Print_Parameters ("Params", Params (index));
               Printing.Print_Parameters ("Num_Grad",
                                          Num_Grad (index).Gradients (1));
            end loop;
         end;
      end loop;

   end loop;

end Test_Gradient;
