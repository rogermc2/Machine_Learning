
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
   Theta              : Parameters_List;
   --      Loss               : Float;
   --      pragma Unreferenced (Loss);

   function Loss_Grad_Function
     (Self        : in out MLP_Classifier; Theta : Parameters_List;
      X           : Real_Float_Matrix; Y         : Boolean_Matrix;
      Activations : Real_Matrix_List;
      Gradients   :  Stochastic_Optimizers.Parameters_List)
      return Loss_Grad_Result is
      Args : Loss_Grad_Args (X'Length, X'Length (2), Y'Length (2));
   begin
      Args.Self := Self;
      Args.Params := Theta;
      Args.X := X;
      Args.Y := Y;
      Args.Activations := Activations;
      Args.Gradients := Gradients;
      Put_Line ("Loss_Grad_Function Args.X'Length" &
                  Integer'Image (Args.X'Length));
      Put_Line ("Args.Y'Length" & Integer'Image (Args.Y'Length));
      Put_Line ("Args.Params (Theta) Length" &
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

      declare
         --  L194
         Y_Bin : constant Boolean_Matrix := Label.Fit_Transform (LB, Y);
      begin
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
            Layer_Units.Append (aClassifier.Attributes.N_Outputs);

            --  L208
            Theta := aClassifier.Attributes.Params;
--              Put_Line (Routine_Name &
--                          "L208 Theta (1).Coeff_Gradients length" &
--                          Integer'Image (Theta (1).Coeff_Gradients'Length));
--              Put_Line (Routine_Name &
--                          "L208 Theta.Intercept_Grads (1) length" &
--                          Integer'Image (Theta (1).Intercept_Grads'Length));
--              Put_Line (Routine_Name &
--                          "L208 Theta (2).Coeff_Gradients length" &
--                          Integer'Image (Theta (2).Coeff_Gradients'Length));
--              Put_Line (Routine_Name &
--                          "L208 Theta.Intercept_Grads (2) length" &
--                          Integer'Image (Theta (2).Intercept_Grads'Length));

            --  L212  Initialize
            Activations.Clear;
            Deltas.Clear;
            Params.Clear;
            Activations.Append (X);
            for layer in 1 .. aClassifier.Attributes.N_Layers - 1 loop
--                 Put_Line (Routine_Name & "L222 layer" & Integer'Image (layer));
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
               end;
            end loop;

            --  L226
            declare
               --  N = Theta_Length
               Theta_Length : constant Positive := Positive (Theta.Length);
               Num_Grad     : array (1 .. Theta_Length) of Loss_Grad_Result;
               Loss_Grad    : Loss_Grad_Result;
            begin
               --  L233 analytically compute the gradients
               Loss_Grad := Loss_Grad_Function (aClassifier, Theta, X, Y_Bin,
                                                Activations, Params);

               --  L239 numerically compute the gradients

               for index in Theta.First_Index .. Theta.Last_Index loop
                  declare
                     dTheta_Length : constant Positive :=
                                       Theta.Element (index).Num_Cols;
                     Eye           : constant Real_Float_Matrix :=
                                       Unit_Matrix (dTheta_Length);
                     dTheta        : Real_Vector (1 .. dTheta_Length);
                  begin
                     --  L240
                     for e_row in dTheta'Range loop
                        dTheta (e_row) := Eye (e_row, index) * Eps;
                     end loop;

                     declare
                        use Parameters_Package;
                        Theta_P      : Parameters_Record :=
                                         Theta.Element (index);
                        Theta_M      : Parameters_Record := Theta_P;
                        Theta_P_List : Parameters_List;
                        Theta_M_List : Parameters_List;
                        Loss_Grad_P  : Loss_Grad_Result;
                        Loss_Grad_M  : Loss_Grad_Result;
                        Grad_Diff    : Parameters_List;
                     begin
--                          Put_Line (Routine_Name &
--                                      "Theta_P.Coeff_Gradients length" &
--                                      Integer'Image (Theta_P.Coeff_Gradients'Length));
--                          Put_Line (Routine_Name &
--                                      "Theta_P.Intercept_Grads length" &
--                                      Integer'Image (Theta_P.Intercept_Grads'Length));
--                          Put_Line (Routine_Name & "dTheata length" &
--                                      Integer'Image (dTheta'Length));
--                          Put_Line (Routine_Name & "Theta length" &
--                                      Integer'Image (Integer (Theta.Length)));
                        Theta_P.Intercept_Grads :=
                          Theta_P.Intercept_Grads + dTheta;
                        Theta_P_List.Append (Theta_P);

                        Theta_M.Intercept_Grads :=
                          Theta_M.Intercept_Grads - dTheta;
                        Theta_M_List.Append (Theta_M);

                        --  L242
                        Loss_Grad_P := Loss_Grad_Function
                          (Self => aClassifier, Theta => Theta_P_List, X => X,
                           Y => Y_Bin, Activations => Activations,
                           Gradients => Params);
                        Loss_Grad_M := Loss_Grad_Function
                          (Self => aClassifier, Theta => Theta_M_List, X => X,
                           Y => Y_Bin, Activations => Activations,
                           Gradients => Params);
                        for index in Loss_Grad_P.Gradients.First_Index ..
                          Loss_Grad_P.Gradients.Last_Index loop
                           Grad_Diff.Append
                             ((Loss_Grad_P.Gradients (index) -
                                Loss_Grad_M.Gradients (index)) / (2.0 * Eps));
                        end loop;
                        Num_Grad (index).Loss :=
                          (Loss_Grad_P.Loss - Loss_Grad_M.Loss) / (2.0 * Eps);
                        Num_Grad (index).Gradients := Grad_Diff;
                     end;
                  end;
               end loop;

               for index in Params.First_Index .. Params.Last_Index loop
                  Printing.Print_Parameters ("Params", Params (index));
                  Printing.Print_Parameters ("Num_Grad",
                                             Num_Grad (index).Gradients (1));
               end loop;
            end;
         end loop;
      end;

   end loop;

end Test_Gradient;
