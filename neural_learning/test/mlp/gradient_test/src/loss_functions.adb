
--  Based on scikit-learn/sklearn/neural_network/tests/test_mlp.py test_gradient
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

package body Loss_Functions is

   function Loss_Grad_Function
     (Self        : in out MLP_Classifier; Theta : Parameters_List;
      X           : Real_Float_Matrix; Y         : Boolean_Matrix;
      Activations : Real_Matrix_List; Gradients   :  Parameters_List)
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

   function Numerical_Loss_Grad
     (aClassifier : in out MLP_Classifier; Theta : Parameters_List;
      X           : Real_Float_Matrix; Y_Bin : Boolean_Matrix;
      Activations : Real_Matrix_List; Params : Parameters_List)
      return Loss_Grad_Array is
      Routine_Name : constant String := "Test_Gradient.Numerical_Loss_Grad ";
      Num_Grad     : Loss_Grad_Array (1 .. Positive (Theta.Length));
   begin
      for index in Theta.First_Index .. Theta.Last_Index loop
         Put_Line (Routine_Name);
         declare
            Coeffs           : constant Real_Float_Matrix :=
                                 Theta.Element (index).Coeff_Gradients;
            Intcepts         : constant Real_Float_Vector :=
                                 Theta.Element (index).Intercept_Grads;
            Grad_Length      : constant Positive := Coeffs'Length;
            Grad_Length2     : constant Positive := Coeffs'Length (2);
            Intcept_Length   : constant Positive := Intcepts'Length;
            dTheta_Grad      : Real_Float_Matrix
              (1 .. Grad_Length, 1 .. Grad_Length2);
            dTheta_Intercept : Real_Float_Vector (1 .. Intcept_Length);
         begin
            --  L240
            for row in dTheta_Grad'Range loop
               for col in dTheta_Grad'Range (2) loop
                  dTheta_Grad (row, col) :=
                    Coeffs (row, col) * Eps;
               end loop;
            end loop;

            for row in dTheta_Intercept'Range loop
               dTheta_Intercept (row) := Intcepts (row) * Eps;
            end loop;

            declare
               use Real_Float_Arrays;
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
               Theta_P.Coeff_Gradients :=
                 Theta_P.Coeff_Gradients + dTheta_Grad;
               Theta_P_List.Append (Theta_P);

               Theta_M.Coeff_Gradients :=
                 Theta_M.Coeff_Gradients - dTheta_Grad;
               Theta_M_List.Append (Theta_M);

               Theta_P.Intercept_Grads :=
                 Theta_P.Intercept_Grads + dTheta_Intercept;
               Theta_P_List.Append (Theta_P);

               Theta_M.Intercept_Grads :=
                 Theta_M.Intercept_Grads - dTheta_Intercept;
               Theta_M_List.Append (Theta_M);

               Put_Line (Routine_Name &
                           "Theta_P.Coeff_Gradients length" &
                           Integer'Image (Theta_P.Coeff_Gradients'Length));
               Put_Line (Routine_Name &
                           "Theta_P.Intercept_Grads length" &
                           Integer'Image (Theta_P.Intercept_Grads'Length));
               Put_Line (Routine_Name & "L242");
               --  L242
               Loss_Grad_P := Loss_Grad_Function
                 (Self => aClassifier, Theta => Theta_P_List, X => X,
                  Y => Y_Bin, Activations => Activations,
                  Gradients => Params);
               Loss_Grad_M := Loss_Grad_Function
                 (Self => aClassifier, Theta => Theta_M_List, X => X,
                  Y => Y_Bin, Activations => Activations,
                  Gradients => Params);
               Put_Line (Routine_Name & "Loss_Grad_M set");
               for index in Loss_Grad_P.Gradients.First_Index ..
                 Loss_Grad_P.Gradients.Last_Index loop
                  Grad_Diff.Append
                    ((Loss_Grad_P.Gradients (index) -
                       Loss_Grad_M.Gradients (index)) / (2.0 * Eps));
               end loop;
               Put_Line (Routine_Name & "Loss_Grad_P.Gradients set");

               Num_Grad (index).Loss :=
                 (Loss_Grad_P.Loss - Loss_Grad_M.Loss) / (2.0 * Eps);
               Num_Grad (index).Gradients := Grad_Diff;
            end;
            Put_Line (Routine_Name & "inner declare done");
         end;
      end loop;

      return Num_Grad;

   end Numerical_Loss_Grad;

end Loss_Functions;
