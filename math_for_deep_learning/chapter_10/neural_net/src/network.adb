
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;
--  with Neural_Utilities;
with Shuffler;
--  with NL_Types;

package body Network is

   --     Neural_Network  : Network_Package.Map;

   procedure Add (Network : in out Network_Data; Layer : Layer_Data) is
   begin
      Network.Layers.Append (Layer);
   end Add;

   --  -------------------------------------------------------------------------

   procedure Fit
     (Network       : in out Network_Data; X_Train : Real_Float_Matrix;
      Y_Train       : Real_Float_Matrix; Minibatches : Positive;
      Learning_Rate : Float; Batch_Size : Positive := 64) is
      X_Batch     : Real_Float_Matrix (1 .. Batch_Size, X_Train'Range (2));
      Y_Batch     : Real_Float_Matrix (1 .. Batch_Size, Y_Train'Range (2));
      Output_Data : Real_Float_Matrix (X_Batch'Range, X_Batch'Range (2));
      Error       : Float;
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

         Output_Data := X_Batch;
         for layer in Network.Layers.First_Index ..
           Network.Layers.Last_Index loop
            Output_Data := Forward (Network.Layers (layer), Output_Data);
         end loop;

         Error := Error + Loss (Y_Batch, Output_Data);

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

end Network;
