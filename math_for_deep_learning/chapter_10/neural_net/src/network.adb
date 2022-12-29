
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with Shuffler;

package body Network is

   procedure Add_Activation_Layer (Network : in out Network_List) is
      Layer : Layer_Data (Activation_Layer);
   begin
      Network.Append (Layer);

   end Add_Activation_Layer;

   --  -------------------------------------------------------------------------

   procedure Add_Fully_Connected_Layer
     (Network : in out Network_List; Input_Size, Output_Size : Positive) is
      Layer : Layer_Data (Hidden_Layer);
      Weights_Row : Real_Float_List;
      use Maths;
   begin
      for row in 1 ..Input_Size loop
         for col in 1 .. Output_Size loop
            Weights_Row.Append (0.5 * Random_Float);
         end loop;
         Layer.Weights.Append (Weights_Row);
      end loop;

      for index in 1 .. Output_Size loop
         Layer.Bias.Append (0.5 * Random_Float);
      end loop;

      Network.Append (Layer);

   end Add_Fully_Connected_Layer;

   --  -------------------------------------------------------------------------

   procedure Fit
     (Network       : in out Network_Data; X_Train : Real_Float_Matrix;
      Y_Train       : Real_Float_Matrix; Minibatches : Positive;
      Learning_Rate : Float; Batch_Size : Positive := 64) is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Network.Fit ";
      X_Batch     : Real_Float_Matrix (1 .. Batch_Size, X_Train'Range (2));
      Y_Batch     : Real_Float_Matrix (1 .. Batch_Size, Y_Train'Range (2));
      Output_Data : Real_Float_List;
      Y_Vector    : Real_Float_Vector (Y_Batch'Range (2));
      Error       : Float;
      Back_Error  : Real_Float_Vector (Y_Vector'Range);
   begin
      for count in 1 .. Minibatches loop
         Put_Line (Routine_Name & "Minibatch" & Integer'Image (count));
         Error := 0.0;
         --  Select a random minibatch
         declare
            Indices : Integer_Array (X_Train'Range);
         begin
            for index in Indices'Range loop
               Indices (index) := index;
            end loop;
            Shuffler.Shuffle (Indices);

            for row in X_Batch'Range loop
               for col in X_Batch'Range (2) loop
                  X_Batch (row, col) := X_Train (Indices (row), col);
               end loop;
               for col in Y_Batch'Range (2) loop
                  Y_Batch (row, col) := Y_Train (Indices (row), col);
               end loop;
            end loop;
         end;  --  declare block

         --  forward propagation
         for sample in X_Batch'Range loop
            Put_Line (Routine_Name & "sample" & Integer'Image (sample));
            for col in X_Batch'Range (2) loop
               Output_Data.Append (X_Batch (Sample, col));
            end loop;
            for col in Y_Batch'Range (2) loop
               Y_Vector (col) := Y_Batch (Sample, col);
            end loop;

            for layer in Network.Layers.First_Index ..
              Network.Layers.Last_Index loop
               Put_Line (Routine_Name & "layer" & Integer'Image (layer));
               Output_Data := Forward (Network.Layers (layer), Output_Data);
            end loop;

            Error := Error + Loss (Y_Vector, Output_Data);
            Put_Line (Routine_Name & "Error" & Float'Image (Error));

            --  backward propagation
            Back_Error := Loss_Deriv (Y_Vector, Output_Data);
            Print_Float_Vector (Routine_Name & "Back_Error", Back_Error);

            for layer in reverse Network.Layers.First_Index ..
              Network.Layers.Last_Index loop
               Put_Line (Routine_Name & "backward layer" &
                           Integer'Image (layer));
               Back_Error := Backward (Network.Layers (layer), Back_Error);
            end loop;
            Put_Line (Routine_Name & "backward layers done");
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
                     Input_Data : Real_Float_Matrix)
                     return Real_Float_List_2D is
      Output_Data : Real_Float_List;
      Predictions : Real_Float_List_2D;
   begin
      --  For each sample
      for row in Input_Data'Range loop
         for col in Input_Data'Range (2) loop
            Output_Data.Append (Input_Data (row, col));
         end loop;

         for layer in Network.Layers.First_Index ..
           Network.Layers.Last_Index loop
            Output_Data := Forward (Network.Layers (layer), Output_Data);
         end loop;
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
