
--  Based on scikit-learn/sklearn/neural_network/tests/test_mlp.py test_gradient
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

package body Loss_Functions is

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
      Params      : Parameters_List) return Real_Float_Vector is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Loss_Functions.Numerical_Loss_Grad ";
      Theta_Length : constant Positive := Positive (Theta.Length);
      Num_Grad     : Real_Float_Vector (1 .. Theta_Length) := (others => 0.0);
   begin
      --  Put_Line (Routine_Name & "Theta length" & Integer'Image (Theta_Length));
      --  Put_Line (Routine_Name & "Theta (1).Coeff_Gradients size" &
      --                    Integer'Image (Theta (1).Coeff_Gradients'Length) & " x"
      --                  & Integer'Image (Theta (1).Coeff_Gradients'Length (2)));
      --  Put_Line (Routine_Name &
      --                    "Theta.Intercept_Grads (1) length" &
      --                    Integer'Image (Theta (1).Intercept_Grads'Length));
      --  Put_Line (Routine_Name &
      --                    "Theta (2).Coeff_Gradients length" &
      --                    Integer'Image (Theta (2).Coeff_Gradients'Length) & " x"
      --                  & Integer'Image (Theta (2).Coeff_Gradients'Length (2)));
      --  Put_Line (Routine_Name &
      --                    "Theta.Intercept_Grads (2) length" &
      --                    Integer'Image (Theta (2).Intercept_Grads'Length));

      for t_index in 1 .. Theta_Length loop
         --           New_Line;
         Put_Line (Routine_Name & "t index" & Integer'Image (t_index));
         declare
            use Parameters_Package;
            Coeffs           : constant Real_Float_Matrix :=
                                 Theta.Element (t_index).Coeff_Gradients;
            Intcepts         : constant Real_Float_Vector :=
                                 Theta.Element (t_index).Intercept_Grads;
            dTheta_Grad      :  constant Real_Float_Matrix :=
                                 Eps * Unit_Matrix (Coeffs'Length);
            dTheta_Intercept :  constant Real_Float_Vector :=
                                 Eps * Unit_Vector (t_index, Intcepts'Length);
            Theta_P          : Parameters_Record := Theta.Element (t_index);
            Theta_M          : Parameters_Record := Theta.Element (t_index);
            Theta_P_List     : Parameters_List;
            Theta_M_List     : Parameters_List;
            Loss_Grad_P      : Loss_Grad_Result;
            Loss_Grad_M      : Loss_Grad_Result;
         begin
            --  L240
            Put_Line (Routine_Name & "L240");
            Put_Line (Routine_Name & "Coeffs size" &
                        Integer'Image (Coeffs'Length) & " x"
                      & Integer'Image (Coeffs'Length (2)));
            Theta_P.Coeff_Gradients := Coeffs + dTheta_Grad;
            Put_Line (Routine_Name & "Coeff_Gradients set");
            Theta_P.Intercept_Grads := Intcepts + dTheta_Intercept;
            Put_Line (Routine_Name & "Intercept_Grads set");
            if t_index = 1 then
               Theta_P_List.Append (Theta_P);
               Theta_P_List.Append (Theta (2));
            else
               Theta_P_List.Append (Theta (1));
               Theta_P_List.Append (Theta_P);
            end if;

            Theta_M.Coeff_Gradients := Coeffs - dTheta_Grad;
            Theta_M.Intercept_Grads := Intcepts - dTheta_Intercept;
            if t_index = 1 then
               Theta_M_List.Append (Theta_M);
               Theta_M_List.Append (Theta (2));
            else
               Theta_M_List.Append (Theta (1));
               Theta_M_List.Append (Theta_M);
            end if;

            --                 Put_Line
            --                   (Routine_Name & "L242 Theta_P.Coeff_Gradients size" &
            --                      Integer'Image (Theta_P.Coeff_Gradients'Length) & " x" &
            --                      Integer'Image (Theta_P.Coeff_Gradients'Length (2)));
            --                 Put_Line (Routine_Name &
            --                             "L242 Theta_P.Intercept_Grads length" &
            --                             Integer'Image (Theta_P.Intercept_Grads'Length));
            --  L242
            Put_Line (Routine_Name & "L242");
            Loss_Grad_P := Loss_Grad_Function
              (Self => aClassifier, Theta => Theta_P_List, X => X,
               Y => Y,  Gradients => Params);
            Loss_Grad_M := Loss_Grad_Function
              (Self => aClassifier, Theta => Theta_M_List, X => X,
               Y => Y, Gradients => Params);
            Num_Grad (t_index) :=
              (Loss_Grad_P.Loss - Loss_Grad_M.Loss) / (2.0 * Eps);
            --                 Put_Line (Routine_Name & "Num_Grad (t_index) set");

            --              Put_Line (Routine_Name & "inner declare done");
         end;
      end loop;

      return Num_Grad;

   end Numerical_Loss_Grad;

end Loss_Functions;
