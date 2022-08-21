
--  Based on scikit-learn/sklearn/neural_network/tests/test_mlp.py test_gradient
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Printing;

package body Loss_Functions is

   --  L228 loss_grad_fun returns mlp._loss_grad_lbfgs [value, grad]
   --  where grad is packed coeffs + intercepts
   function Loss_Grad_Function
     (Self        : MLP_Classifier; Theta : Parameters_List;
      X           : Real_Float_Matrix; Y  : Binary_Matrix;
      Gradients   :  Parameters_List) return Loss_Grad_Result is
      Activations : Real_Matrix_List;
      Args        : Loss_Grad_Args (X'Length, X'Length (2), Y'Length (2));
   begin
      Activations.Append (X);
      Args.Self := Self;
      Args.Params := Theta;
      Args.X := X;
      Args.Y := Y;
      Args.Activations := Activations;
      Args.Gradients := Gradients;

      return Loss_Grad_LBFGS (Args);

   end Loss_Grad_Function;

   --  -------------------------------------------------------------------------

   function Numerical_Loss_Grad
     (aClassifier : MLP_Classifier; Theta : Parameters_List;
      X           : Real_Float_Matrix; Y  : Binary_Matrix;
      Params      : Parameters_List) return Parameters_List is
      Routine_Name : constant String := "Loss_Functions.Numerical_Loss_Grad ";
      Theta_Length : constant Positive := Positive (Theta.Length);
      Num_Grad     : Parameters_List;
   begin
      for t_index in 1 .. Theta_Length loop
         declare
            use Parameters_Package;
            Coeffs           : constant Real_Float_Matrix :=
                                 Theta.Element (t_index).Coeff_Gradients;
            Intcepts         : constant Real_Float_Vector :=
                                 Theta.Element (t_index).Intercept_Grads;
            Theta_P          : Parameters_Record := Theta.Element (t_index);
            Theta_M          : Parameters_Record := Theta.Element (t_index);
            Theta_P_List     : Parameters_List;
            Theta_M_List     : Parameters_List;
            Loss_Grad_P      : Loss_Grad_Result;
            Loss_Grad_M      : Loss_Grad_Result;
            Coeff_Diff       : Float;
            Grad_Diff        : Parameters_Record (Coeffs'Length, Coeffs'Length (2));
         begin
            Printing.Print_Matrix_Dimensions
              (Routine_Name & "Coeffs (" & Integer'Image (t_index) & ")",
               Coeffs);
            --  L240
            Theta_P.Coeff_Gradients := Coeffs;
            Theta_P.Intercept_Grads := Intcepts;
            Theta_M.Coeff_Gradients := Coeffs;
            Theta_M.Intercept_Grads := Intcepts;

            for row in Coeffs'Range loop
               for col in Coeffs'Range (2) loop
                  if col = t_index then
                     Theta_P.Coeff_Gradients (row, col) :=
                       Theta_P.Coeff_Gradients (row, col) + Eps;
                     Theta_M.Coeff_Gradients (row, col) :=
                       Theta_M.Coeff_Gradients (row, col) - Eps;
                  end if;
               end loop;

               if row = t_index then
                  Theta_P.Intercept_Grads (row) :=
                    Theta_P.Intercept_Grads (row) + Eps;
                  Theta_M.Intercept_Grads (row) :=
                    Theta_M.Intercept_Grads (row) - Eps;
               end if;
            end loop;

            for row in Coeffs'Range loop
               for col in Coeffs'Range (2) loop
                  if col = t_index then
                     Theta_P.Coeff_Gradients (row, col) :=
                       Theta_P.Coeff_Gradients (row, col) + Eps;
                  end if;
               end loop;

               if row = t_index then
                  Theta_P.Intercept_Grads (row) :=
                    Theta_P.Intercept_Grads (row) + Eps;
               end if;
            end loop;

            if t_index = 1 then
               Theta_P_List.Append (Theta_P);
               Theta_P_List.Append (Theta (2));
            else
               Theta_P_List.Append (Theta (1));
               Theta_P_List.Append (Theta_P);
            end if;

            if t_index = 1 then
               Theta_M_List.Append (Theta_M);
               Theta_M_List.Append (Theta (2));
            else
               Theta_M_List.Append (Theta (1));
               Theta_M_List.Append (Theta_M);
            end if;

            --  L242 loss_grad_fun returns mlp._loss_grad_lbfgs [value, grad]
            --  where grad is packed coeffs + intercepts
            Loss_Grad_P := Loss_Grad_Function
              (Self => aClassifier, Theta => Theta_P_List, X => X,
               Y => Y,  Gradients => Params);
            Loss_Grad_M := Loss_Grad_Function
              (Self => aClassifier, Theta => Theta_M_List, X => X,
               Y => Y, Gradients => Params);
            for col in Coeffs'Range (2) loop
               Coeff_Diff :=
                 (Loss_Grad_P.Gradients (col).Coeff_Gradients (1, col) -
                      Loss_Grad_M.Gradients (1).Coeff_Gradients (1, 1)) /
                   (2.0 * Eps);
            end loop;
            Grad_Diff.Intercept_Grads (t_index) :=
              (Loss_Grad_P.Gradients(1).Intercept_Grads (1) -
                   Loss_Grad_M.Gradients (1).Intercept_Grads (1)) /
                (2.0 * Eps);
            Grad_Diff.Coeff_Gradients (t_index, 1) := Coeff_Diff;
            Num_Grad.Append (Grad_Diff);
         end;
      end loop;
      Printing.Print_Parameters (Routine_Name & "Num_Grad (1)",
                                   Num_Grad.Element (1));

      return Num_Grad;

   end Numerical_Loss_Grad;

end Loss_Functions;
