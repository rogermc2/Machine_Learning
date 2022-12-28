
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Shuffler;

package body Network is

   --     Num_Samples : Positive;

   procedure Add_Activation_Layer (Network : in out Network_List) is
      Layer : Layer_Data (Activation_Layer, 0, 0);
   begin
      Network.Append (Layer);

   end Add_Activation_Layer;

   --  -------------------------------------------------------------------------

   procedure Add_Fully_Connected_Layer
     (Network : in out Network_List; Input_Size, Output_Size : Positive) is
      Layer : Layer_Data (Hidden_Layer, Input_Size, Output_Size);
      use Maths;
   begin
      for row in Layer.Weights'Range loop
         for col in Layer.Weights'Range (2) loop
            Layer.Weights (row, col) := 0.5 * Random_Float;
         end loop;
      end loop;

      for col in Layer.Bias'Range (2) loop
         Layer.Bias (1, col) := 0.5 * Random_Float;
      end loop;

      Network.Append (Layer);

   end Add_Fully_Connected_Layer;

   --  -------------------------------------------------------------------------

   procedure Fit
     (Network       : in out Network_Data; X_Train : Real_Float_Matrix;
      Y_Train       : Real_Float_Matrix; Minibatches : Positive;
      Learning_Rate : Float; Batch_Size : Positive := 64) is
      X_Batch     : Real_Float_Matrix (1 .. Batch_Size, X_Train'Range (2));
      Y_Batch     : Real_Float_Matrix (1 .. Batch_Size, Y_Train'Range (2));
      Output_Data : Real_Float_Matrix (1 .. Batch_Size, X_Batch'Range (2));
      Error       : Float;
      Back_Error  : Real_Float_Matrix (1 .. Batch_Size, X_Batch'Range (2));
   begin
      for count in 1 .. Minibatches loop
         Error := 0.0;
         --  Select a random minibatch
         declare
            Indices : Integer_Array (X_Train'Range);
         begin
            Shuffler.Shuffle (Indices);
            for row in X_Batch'Range loop
               for col in X_Batch'Range (2) loop
                  X_Batch (row, col) := X_Train (Indices (row), col);
                  Y_Batch (row, col) := Y_Train (Indices (row), col);
               end loop;
            end loop;
         end;  --  declare block

         --  forward propagation
         Output_Data := X_Batch;

         for layer in Network.Layers.First_Index ..
           Network.Layers.Last_Index loop
            Output_Data := Forward (Network.Layers (layer), Output_Data);
         end loop;

         Error := Error + Loss (Y_Batch, Output_Data);

         --  backward propagation
         Back_Error := Loss_Deriv (Y_Batch, Output_Data);

         for layer in reverse Network.Layers.First_Index ..
           Network.Layers.Last_Index loop
            Back_Error := Backward (Network.Layers (layer), Back_Error);
         end loop;

         --  update weights and biases
         for layer in Network.Layers.First_Index ..
           Network.Layers.Last_Index loop
            Step (Network.Layers (layer), Learning_Rate);
         end loop;

         --  report mean loss over minibatch
         if Network.Verbose and then count mod 10 = 0 then
            Put_Line ("Minibatch" & Integer'Image (count) &
                        " error: " & Float'Image (Error));
         end if;
      end loop;

   end Fit;

   --  -------------------------------------------------------------------------

   function Predict (Network    : in out Network_Data;
                     Input_Data : Real_Float_Matrix) return Real_Matrix_List is
      Output_Data : Real_Float_Matrix := Input_Data;
      Predictions : Real_Matrix_List;
   begin
      for layer in Network.Layers.First_Index ..
        Network.Layers.Last_Index loop
         Output_Data := Forward (Network.Layers (layer), Output_Data);
         Predictions.Append (Output_Data);
      end loop;

      return Predictions;

   end Predict;

   --  -------------------------------------------------------------------------

   --     procedure Set_Num_Samples (Number : Positive) is
   --     begin
   --        Num_Samples := Number;
   --     end Set_Num_Samples;

   --  -------------------------------------------------------------------------

end Network;
