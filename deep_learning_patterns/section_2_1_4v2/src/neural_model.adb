
with Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base_Neural;
with Basic_Printing; use Basic_Printing;
with Neural_Maths;
with Stochastic_Optimizers; use Stochastic_Optimizers;

package body Neural_Model is

   procedure Backward (aModel          : in out Sequential_Model;
                       Sample, L_Index : Positive);
   --     function Backward (aModel          : in out Sequential_Model;
   --                        Sample, L_Index : Positive;
   --                        Output_Error    : Real_Float_Vector)
   --                        return Real_Float_Vector;
   procedure Compute_Coeff_Gradient (aModel      : in out Sequential_Model;
                                     Layer_Index : Positive;
                                     Loss_Deriv  : Real_Float_Matrix);
   procedure Compute_Intercept_Gradient (aModel      : in out Sequential_Model;
                                         Layer_Index : Positive;
                                         Loss_Deriv  : Real_Float_Matrix);
   function Deriv_ReLU (X : Real_Float_Vector) return Real_Float_Vector;
   function Deriv_Softmax (X : Real_Float_Vector) return Real_Float_Matrix;
   procedure Forward (aModel : in out Sequential_Model);
   function To_Matrix (Data : Real_Float_Vector) return Real_Float_Matrix;

   --  -------------------------------------------------------------------------

   procedure Add_Connections (aModel : in out Sequential_Model) is
      use Real_Float_Arrays;
      use Stochastic_Optimizers;
      Routine_Name : constant String := "Neural_Model.Add_Connections ";
   begin
      for layer in aModel.Layers.First_Index ..
        aModel.Layers.Last_Index - 1 loop
         declare
            Connect : Parameters_Record (aModel.Layers (layer + 1).Num_Nodes,
                                         aModel.Layers (layer).Num_Nodes);
         begin
            for row in Connect.Coeff_Gradients'Range loop
               for col in Connect.Coeff_Gradients'Range (2) loop
                  --  Random_Float generates a random number in the range  -1.0 .. 1.0
                  Connect.Coeff_Gradients (row, col) :=
                    0.5 * abs (Maths.Random_Float);
               end loop;
               Connect.Intercept_Grads (row)  := 0.5 * abs (Maths.Random_Float);
            end loop;

            aModel.Connections.Append (Connect);
         end;  --  declare block
      end loop;

   end Add_Connections;

   --  ---------------------------------------------------------------------------

   --  Initialization - add first layer
   procedure Add_First_Layer (aModel     : in out Sequential_Model;
                              Input_Data : Real_Float_Matrix) is
      First_Layer : Layer (Input_Data'Length, Input_Data'Length (2),
                           Input_Data'Length (2));
   begin
      First_Layer.Input_Data := Input_Data;
      First_Layer.Nodes := Input_Data;
      aModel.Layers.Append (First_Layer);

   end Add_First_Layer;

   --  ---------------------------------------------------------------------------
   --  Initialization - add other layers
   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive;
                        Activation : Activation_Kind := Identity_Activation) is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Model.Add_Layer others  ";
      Prev_Layer   : constant Layer := aModel.Layers.Last_Element;
      Prev_Nodes   : constant Real_Float_Matrix := Prev_Layer.Nodes;
      thisLayer    : Layer (aModel.Num_Samples, Prev_Nodes'Length (2),
                            Num_Nodes);
   begin
      thisLayer.Input_Data := Prev_Nodes;
      thisLayer.Activation := Activation;
      aModel.Layers.Append (thisLayer);

   end Add_Layer;

   --  -------------------------------------------------------------------------

   procedure Back_Propogate
     (aModel    : in out Sequential_Model;
      Optimiser : Stochastic_Optimizers.Optimizer_Record) is
      use Stochastic_Optimizers;
      use Real_Float_Arrays;
      use Real_Matrix_List_Package;
      Routine_Name : constant String := "Neural_Model.Back_Propogate ";
   begin
      for sample in 1 .. aModel.Num_Samples loop
         Put_Line (Routine_Name & "sample " & Integer'Image (sample));
         for layer_id in reverse
           aModel.Layers.First_Index .. aModel.Layers.Last_Index loop
            Put_Line (Routine_Name & "layer" & Integer'Image (layer_id));
            Print_Matrix_Dimensions (Routine_Name & "Input_Data",
                                     aModel.Layers (layer_id).Input_Data);
            if layer_id < aModel.Layers.Last_Index then
               Backward (aModel, sample, layer_id);
            end if;
         end loop;
      end loop;

   end Back_Propogate;

   --  -------------------------------------------------------------------------

   procedure Backward (aModel          : in out Sequential_Model;
                       Sample, L_Index : Positive) is
      use Real_Float_Arrays;
      Routine_Name  : constant String := "Neural_Model.Backward ";
      Prev_Layer    : Layer := aModel.Layers (L_Index + 1);
      This_Layer    : Layer := aModel.Layers (L_Index);
      --  Transpose of Coeff_Gradients is dX/dY
      Input_Error   : constant Real_Float_Vector
        := Prev_Layer.Output_Error *
          Transpose (aModel.Connections (L_Index - 1).Coeff_Gradients);
--        Weights_Error : constant Real_Float_Vector :=
--                          Prev_Layer.Output_Error *
--                            Transpose (This_Layer.Input_Data);
--        D_Weights     : Real_Float_Vector :=
--                          Get_Row (This_Layer.Delta_Weights, Sample);
   begin
      Put_Line (Routine_Name);
--        Put_Line (Routine_Name & "D_Weights length " &
--                    Integer'Image (D_Weights'Length));
      Put_Line (Routine_Name & "Input_Error length " &
                  Integer'Image (Input_Error'Length));
--        D_Weights := D_Weights + Input_Error;
--
--        for col in D_Weights'Range loop
--           This_Layer.Delta_Weights (Sample, col) := D_Weights (col);
--        end loop;

      for col in Prev_Layer.Output_Error'Range loop
         This_Layer.Delta_Bias (Sample, col) :=
           This_Layer.Delta_Bias (Sample, col) + Prev_Layer.Output_Error (col);
      end loop;

      aModel.Layers (L_Index) := This_Layer;

   end Backward;

   --  -------------------------------------------------------------------------

   --     function Backward (aModel          : in out Sequential_Model;
   --                        Sample, L_Index : Positive;
   --                        Output_Error    : Real_Float_Vector)
   --                        return Real_Float_Vector is
   --        use Real_Float_Arrays;
   --        Routine_Name  : constant String := "Neural_Model.Backward ";
   --        aLayer        : Layer := aModel.Layers (L_Index);
   --        --  Transpose of Coeff_Gradients is dX/dY
   --        Input_Error   : constant Real_Float_Vector
   --          := Output_Error *
   --            Transpose (aModel.Connections (L_Index - 1).Coeff_Gradients);
   --        Weights_Error : constant Real_Float_Vector :=
   --                          Output_Error * Transpose (aLayer.Input_Data);
   --        D_Weights     : Real_Float_Vector :=
   --                          Get_Row (aLayer.Delta_Weights, Sample);
   --     begin
   --        --           Compute_Coeff_Gradient (aModel, index, Loss_Deriv);
   --        --           Compute_Intercept_Gradient (aModel, index, Loss_Deriv);
   --        Put_Line (Routine_Name);
   --        Put_Line (Routine_Name & "D_Weights length " &
   --                    Integer'Image (D_Weights'Length));
   --        Put_Line (Routine_Name & "Input_Error length " &
   --                    Integer'Image (Input_Error'Length));
   --        D_Weights := D_Weights + Input_Error;
   --        for col in D_Weights'Range loop
   --           aLayer.Delta_Weights (Sample, col) := D_Weights (col);
   --        end loop;
   --
   --        for col in Output_Error'Range loop
   --           aLayer.Delta_Bias (Sample, col) :=
   --             aLayer.Delta_Bias (Sample, col) + Output_Error (col);
   --        end loop;
   --
   --        aModel.Layers (L_Index) := aLayer;
   --
   --        return Input_Error;
   --
   --     end Backward;

   --  -------------------------------------------------------------------------

   function Backward_Activation (aModel : in out Sequential_Model;
                                 Loss   : Real_Float_Vector)
                                 return Real_Float_Vector is
      use Real_Float_Arrays;
      --        Routine_Name        : constant String :=
      --                                "Neural_Model.Backward_Activation ";
   begin
      --  From Multilayer_Perceptron.Update_Hidden_Layer_Gradients:
      --  Rect_LU_Derivative (Activations (Layer), Deltas (Layer - 1));
      --        return Neural_Maths.Sigmoid_Deriv (Loss);
      return Deriv_ReLU (Loss);

   end Backward_Activation;

   --  -------------------------------------------------------------------------

   procedure Compile (aModel : in out Sequential_Model) is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Model.Compile ";
      --  Loss_Deriv is dE/dY for output layer
      Loss         : Float := 0.0;
      Optimiser    : Optimizer_Record (Optimizer_Adam);
      Params       : Parameters_List;  --  list of Parameters_Record
   begin
      C_Init (Optimiser.Adam, aModel.Connections);
      Forward (aModel);

      Back_Propogate (aModel, Optimiser);

      --           declare
      --              Input_Error : Real_Float_Vector
      --                :=  Backward (aModel, sample, Get_Row (Loss_Deriv, sample));
      --           begin
      --              Put_Line (Routine_Name & "sample " & Integer'Image (sample) &
      --                          " Input_Error length" &
      --                          Integer'Image (Input_Error'Length));
      --              Print_Float_Vector (Routine_Name & "Input_Error ", Input_Error);
      --           end;
      --        Gradients := Back_Propogate (aModel, Optimiser, Loss, Loss_Deriv);
      --        end loop;

   end Compile;

   --  -------------------------------------------------------------------------

   procedure Compute_Coeff_Gradient (aModel      : in out Sequential_Model;
                                     Layer_Index : Positive;
                                     Loss_Deriv  : Real_Float_Matrix) is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Model.Compute_Coeff_Gradient ";
      Deriv_In     : constant Real_Float_Matrix := Loss_Deriv *
                       Transpose
                         (aModel.Connections (Layer_Index).Coeff_Gradients);
      aLayer       : Layer := aModel.Layers (Layer_Index);
      Weights_Err  : constant Real_Float_Vector :=
                       Get_Row (aLayer.Input_Data, 1) * Loss_Deriv;
      Nodes        : Real_Float_Matrix := aLayer.Nodes;
   begin
      aLayer.Delta_Weights := aLayer.Delta_Weights + Weights_Err;
      aLayer.Delta_Bias := aLayer.Delta_Bias + Loss_Deriv;
      aModel.Layers (Layer_Index) := aLayer;

   end Compute_Coeff_Gradient;

   --  -------------------------------------------------------------------------

   procedure Compute_Intercept_Gradient (aModel      : in out Sequential_Model;
                                         Layer_Index : Positive;
                                         Loss_Deriv  : Real_Float_Matrix) is
      Routine_Name : constant String :=
                       "Neural_Model.Compute_Intercept_Gradient ";
      aLayer       : Layer := aModel.Layers (Layer_Index);
      Nodes        : constant Real_Float_Matrix := aLayer.Nodes;
      Deriv_Matrix : Real_Float_Matrix (Nodes'Range, Nodes'Range);
   begin
      Put_Line (Routine_Name & "Activation: " &
                  Activation_Kind'Image (aLayer.Activation));
      for row in Nodes'Range loop
         case aLayer.Activation is
            when Identity_Activation => null;
            when Logistic_Activation => null;
            when ReLu_Activation =>
               aModel.Connections (Layer_Index).Intercept_Grads :=
                 Deriv_ReLU (Get_Row (Nodes, row));
            when Sigmoid_Activation => null;
            when Soft_Max_Activation =>
               Deriv_Matrix := Deriv_Softmax (Get_Row (Nodes, row));
               if Nodes'Length = 1 then
                  aModel.Connections (Layer_Index).Intercept_Grads (1) := 0.0;
               else
                  Put_Line (Routine_Name & "Soft_Max_Activation incomplete.");
               end if;
         end case;
      end loop;

      aModel.Layers (Layer_Index) := aLayer;

   end Compute_Intercept_Gradient;

   --  -------------------------------------------------------------------------

   function Deriv_ReLU (X : Real_Float_Vector) return Real_Float_Vector is
      RLU   : Real_Float_Vector := X;
      Deriv : Real_Float_Vector (X'Range);
   begin
      Base_Neural.Rect_LU (RLU);
      for index in X'Range loop
         if X (index) <= 0.0 then
            Deriv (index) := 0.0;
         else
            Deriv (index) := RLU (index) / X (index);
         end if;
      end loop;

      return RLU;

   end Deriv_ReLU;

   --  -------------------------------------------------------------------------
   --  When calculating the derivative of the softmax function,
   --  a Jacobian matrix which is the matrix of all first-order
   --  partial derivatives is needed.
   function Deriv_Softmax (X : Real_Float_Vector) return Real_Float_Matrix is
      --        Routine_Name : constant String := "Neural_Model.Deriv_Softmax ";
      Jacobian     : Real_Float_Matrix (X'Range, X'Range);
   begin
      for row in Jacobian'Range loop
         for col in Jacobian'Range (2) loop
            if col = row then
               Jacobian (row, col) := X (col) * (1.0 - X (col));
            else
               Jacobian (row, col) := -X (col) * X (row);
            end if;
         end loop;
      end loop;

      return Jacobian;

   end Deriv_Softmax;

   --  -------------------------------------------------------------------------

   procedure Forward (aModel : in out Sequential_Model) is
      use Real_Float_Arrays;
      use Base_Neural;
      use Stochastic_Optimizers;
      use Layer_Packge;
      Routine_Name : constant String := "Neural_Model.Forward ";
      Loss         : Real_Float_Vector (1 .. aModel.Num_Samples) :=
                       (others => 0.0);
   begin
      Put_Line (Routine_Name & "Num layers:" &
                  Integer'Image (Integer (aModel.Layers.Length)));
      --        Print_Float_Matrix (Routine_Name & "Layer 1 nodes",
      --                            aModel.Layers (1).Nodes);

      for layer in aModel.Layers.First_Index + 1 ..
        aModel.Layers.Last_Index loop
         Put_Line (Routine_Name & "layer" & Integer'Image (layer));
         declare
            Connect : constant Parameters_Record :=
                        aModel.Connections (layer - 1);
         begin
            aModel.Layers (layer).Input_Data := aModel.Layers (layer - 1).Nodes;
            Print_Float_Matrix (Routine_Name & "Input_Data",
                                aModel.Layers (layer).Input_Data);
            for sample in 1 .. aModel.Num_Samples loop
               Put_Line (Routine_Name & "sample" & Integer'Image (sample));
               declare
                  Input_Vec : constant Real_Float_Vector
                    := Get_Row (aModel.Layers (layer - 1).Nodes, sample);
                  Update    : constant Real_Float_Vector
                    := Connect.Coeff_Gradients * Input_Vec +
                      Connect.Intercept_Grads;
               begin
                  for col in Input_Vec'Range loop
                     aModel.Layers (layer).Input_Data (sample, col) :=
                       Input_Vec (col);
                  end loop;
                  for col in Update'Range loop
                     aModel.Layers (layer).Nodes (sample, col) :=
                       Update (col);
                  end loop;
               end;
            end loop;  --  samples
            Print_Float_Matrix (Routine_Name & "Input_Data",
                                aModel.Layers (layer).Input_Data);
            Print_Float_Matrix (Routine_Name & "nodes",
                                aModel.Layers (layer).Nodes);

            case aModel.Layers (layer).Activation is
               when Identity_Activation => null;
               when Logistic_Activation =>
                  Put_Line (Routine_Name & "Logistic_Activation not implemented");
               when ReLu_Activation => Rect_LU (aModel.Layers (layer).Nodes);
               when Sigmoid_Activation =>
                  Put_Line (Routine_Name & "Sigmoid_Activation not implemented");
               when Soft_Max_Activation => Softmax (aModel.Layers (layer).Nodes);
            end case;

            --              Print_Float_Matrix (Routine_Name & "Layer" &
            --                                    Integer'Image (layer) & " nodes",
            --                                  aModel.Layers (layer).Nodes);
         end;  --  declare block
      end loop;  --  layers

      for sample in 1 .. aModel.Num_Samples loop
         for label in aModel.Labels'Range loop
            aModel.Pred (sample, label) :=
              aModel.Layers.Last_Element.Nodes (sample, label);
         end loop;

         case aModel.Loss_Method is
            when Loss_Binary_Log =>
               Put_Line (Routine_Name & "Binary_Log_Loss method not implemented");
            when Loss_Log =>
               Put_Line (Routine_Name & "Log_Loss method not implemented");
            when Loss_Mean_Square_Error =>
               Loss (sample) := Base_Neural.Mean_Squared_Error (aModel.Pred,
                                                                aModel.Labels);
               --  Output_Error is dE/dY for output layer
               declare
                  Pred       : constant Real_Float_Vector :=
                                 Get_Row (aModel.Pred, sample);
                  Actual     : constant Real_Float_Vector :=
                                 Get_Row (aModel.Labels, sample);
                  Last_Layer : Layer := aModel.Layers.Last_Element;
               begin
                  Last_Layer.Output_Error :=
                    Base_Neural.MSE_Derivative (Pred, Actual) *
                  Transpose (aModel.Connections.Last_Element.Coeff_Gradients);
                  aModel.Layers (aModel.Layers.Last_Index) := Last_Layer;
               end;
         end case;
      end loop;

   end Forward;

   --  ---------------------------------------------------------------------------

   --     function Get_Output_Value (aModel : Sequential_Model) return Real_Float_Vector is
   --     begin
   --        return aModel.Layers.Last_Element.Output_Data;
   --
   --     end Get_Output_Value;

   --  -------------------------------------------------------------------------

   function To_Matrix (Data : Real_Float_Vector) return Real_Float_Matrix is
      Result   : Real_Float_Matrix (1 .. 1, Data'Range);
   begin
      for col in Data'Range loop
         Result (1, col) := Data (col);
      end loop;

      return Result;

   end To_Matrix;

   --  ---------------------------------------------------------------------------

end Neural_Model;
