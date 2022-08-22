
--  Based on scikit-learn/sklearn/neural_network/tests/test_mlp.py test_gradient
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Printing;

package body Loss_Functions2 is

--      Epsilon : constant Float := 10.0 ** (-5);

    function Pack (Theta : Parameters_List) return Real_Float_Vector;

    --  -------------------------------------------------------------------------

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
       Params      : Parameters_List) return Real_Float_Vector is
        use Real_Float_Arrays;
--          Routine_Name  : constant String :=
--                            "Loss_Functions2.Numerical_Loss_Grad ";
        Epsilon       : constant Float := 10.0 ** (-5);
        Theata_Vec    : constant Real_Float_Vector := Pack (Theta);
        D_Theta       : constant Real_Float_Matrix :=
                          Epsilon * Unit_Matrix (Theata_Vec'Length);
        Loss_Grad_P   : Loss_Grad_Result;
        Loss_Grad_M   : Loss_Grad_Result;
        Num_Grad      : Real_Float_Vector (Theata_Vec'Range) := (others => 0.0);
    begin
        for t_index in Theata_Vec'Range loop
            --  L240
            --  L242 loss_grad_fun returns mlp._loss_grad_lbfgs [, grad]
            --  where grad is packed coeffs + intercepts
            Loss_Grad_P := Loss_Grad_Function
              (Self => aClassifier, Theta => Theata_Vec + D_Theta, X => X,
               Y => Y,  Gradients => Params);
            Loss_Grad_M := Loss_Grad_Function
              (Self => aClassifier, Theta => Theata_Vec - D_Theta, X => X,
               Y => Y, Gradients => Params);
            Num_Grad (t_index) := (Loss_Grad_P.Loss - Loss_Grad_M.Loss) /
              (2.0 * Epsilon);
        end loop;

        return Num_Grad;

    end Numerical_Loss_Grad;

    --  -------------------------------------------------------------------------

    function Pack (Theta : Parameters_List) return Real_Float_Vector is
        Pack_List : Real_Float_List;
    begin
        for index in Theta.First_Index .. Theta.Last_Index loop
            declare
                Param : constant Parameters_Record := Theta (index);
            begin
                for row in Param.Coeff_Gradients'Range loop
                    for col in Param.Coeff_Gradients'Range (2) loop
                        Pack_List.Append (Param.Coeff_Gradients (row, col));
                    end loop;
                end loop;

                for row in Param.Coeff_Gradients'Range loop
                    Pack_List.Append (Param.Intercept_Grads (row));
                end loop;
            end;
        end loop;

        return To_Real_Float_Vector (Pack_List);

    end Pack;

    --  -------------------------------------------------------------------------

end Loss_Functions2;
