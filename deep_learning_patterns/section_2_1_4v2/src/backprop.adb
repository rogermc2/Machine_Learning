
with Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Stochastic_Optimizers;

package body Backprop is
   --  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
   --  L241
   --  Back_Propogate computes the loss function and its derivatives
   --  with respect to each parameter: weights and bias vectors.
   function Back_Propogate (aModel      : Sequential_Model;
                             Optimiser  : Stochastic_Optimizers.Optimizer_Record;
                            Loss_Method : Loss_Kind)
                            return Stochastic_Optimizers.Parameters_List is
      use Ada.Assertions;
      use Ada.Containers;
      use Base_Neural;
      --        use Parameters_Package;
      use Stochastic_Optimizers;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      Routine_Name       : constant String :=
                             "Structure_V2.Back_Propogate ";
      Pred               : constant Real_Float_Matrix :=
                             To_Real_Float_Matrix (aModel.Labels);
      Loss_Function_Name : Loss_Kind;
      Loss               : Float;
      Deltas             : Real_Matrix_List;
      Sum_Sq_Coeffs      : Float := 0.0;
      Predd_Gradients  : Parameters_List;
   begin
      --  L284
--        Assert (Pred'Length = Integer (aModel.Activations.Last_Element.Length),
--                Routine_Name & "L284 unequal Pred and Activations lengths");
--        Assert (Pred'Length (2) = aModel.Activations.Last_Element'Length (2),
--                Routine_Name & "L284 Pred has a different number of columns" &
--                  Integer'Image (Pred'Length (2)) & " to" &
--                  " Activations.Last_Element columns" &
--                  Integer'Image (Activations.Last_Element'Length (2)));

      if aModel.Loss_Method = Loss_Log and then
        aModel.Activations.Last_Element = Logistic_Activation then
         Loss_Function_Name := Loss_Binary_Log;
      else
         Loss_Function_Name := aModel.Loss_Method;
      end if;

--        case Loss_Function_Name is
--           when Loss_Binary_Log =>
--              Loss := Binary_Log_Loss (Pred, aModel.Activations.Last_Element);
--           when Loss_Log =>
--              Loss := Log_Loss (Pred,  aModel.Activations.Last_Element);
--           when Loss_Mean_Square_Error =>
--              Loss := Mean_Squared_Error (Pred,  aModel.Activations.Last_Element);
--        end case;

      --  L292  Add L2 regularization term to loss
      --  for s in self.coefs_:
      for layer in aModel.Connections.First_Index ..
        aModel.Connections.Last_Index loop
         declare
            Coeffs : constant Real_Float_Matrix :=
                       aModel.Connections (Layer).Coeff_Gradients;
         begin
            for row in Coeffs'Range loop
               for col in Coeffs'Range (2) loop
                  Sum_Sq_Coeffs := Sum_Sq_Coeffs + Coeffs (row, col) ** 2;
               end loop;
            end loop;
         end;  --  declare
      end loop;

      --  L292
--        Loss := Loss + 0.5 * (MLP.Parameters.Alpha *
--                                Sum_Sq_Coeffs / Float (aModel.Num_Samples));

      --  L297 Backward propagate
      --  The calculation of delta[last]  works with the following
      --  combinations of output activation and loss function:
      --  sigmoid and binary cross entropy, softmax and categorical cross
      --  entropy and identity with squared loss.
      --  The ith element of deltas holds the difference between the
      --  activations of the i + 1 layer and the backpropagated error.
      --  Deltas are gradients of loss with respect to z in each layer.
      --  The ith element of Activations (batch samples x classes) is a list
      --  of the ith layer class values of a batch.

      --  L301  Initialize Deltas
      Deltas.Set_Length (Count_Type (aModel.Layers.Length - 1));
--        Assert (Activations.Last_Element'Length (2) = Pred'Length (2),
--                Routine_Name & "L301 last Activations item width" &
--                  Integer'Image (Activations.Last_Element'Length (2)) &
--                  " differs from Pred width" &
--                  Integer'Image (Pred'Length (2)));
      --  L301  310
      Deltas.Replace_Element (Deltas.Last_Index,
                              Activations.Last_Element - Pred);
      Predd_Gradients.Set_Length (Count_Type (aModel.Layers.Length));

      --  L304 Compute gradient for the last layer
      declare
         Grad : constant Parameters_Record :=
                  Compute_Loss_Gradient (MLP, MLP.Attributes.N_Layers - 1,
                                         Num_Samples, Activations, Deltas);
      begin
         Predd_Gradients.Replace_Element
           (Predd_Gradients.Last_Index, Grad);
      end;

      --  L310, L308
      Pred_Hidden_Layer_Gradients
        (MLP, Activations, Deltas, Predd_Gradients, Num_Samples);

      return Predd_Gradients;

   end Back_Propogate;

   --  -------------------------------------------------------------------------

end Backprop;
