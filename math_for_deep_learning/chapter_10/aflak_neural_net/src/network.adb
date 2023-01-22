
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with Losses;

package body Network is

   --  -------------------------------------------------------------------------

   procedure Add_Tanh_Layer (Network : in out Network_List) is
      Layer : Layer_Data (Tanh_Layer, 0, 0);
   begin
      Network.Append (Layer);

   end Add_Tanh_Layer;

   --  -------------------------------------------------------------------------

   procedure Add_Tanh_Layer (Network    : in out Network_List;
                                   Input_Size : Layer_Range) is
      Layer : Layer_Data (Tanh_Layer, Input_Size, 0);
   begin
      Network.Append (Layer);

   end Add_Tanh_Layer;

   --  -------------------------------------------------------------------------

   procedure Add_Dense_Layer
     (Network : in out Network_List; Input_Size, Output_Size : Layer_Range) is
      Layer : Layer_Data (Dense_Layer, Input_Size, Output_Size);
      use Maths;
   begin
      for row in 1 .. Output_Size loop
         for col in 1 .. Input_Size loop
            Layer.Weights (Row, Col) := Random_Float;
         end loop;
      end loop;

      for row in 1 .. Output_Size loop
         Layer.Bias (row, 1) := Random_Float;
      end loop;

      Network.Append (Layer);

   end Add_Dense_Layer;

   --  -------------------------------------------------------------------------

   procedure Predict (Network : in out Network_Data;
                      Data    : in out Real_Float_List) is
--        Routine_Name : constant String := "Network.Predict ";
   begin
      for layer in Network.Layers.First_Index ..
        Network.Layers.Last_Index loop
         Forward (Network.Layers (layer), Data);
      end loop;

   end Predict;

   --  -------------------------------------------------------------------------

   procedure Print_Layer_Data (Name : String; Layer : Layer_Data) is
   begin
      Put_Line (Name);
      Put_Line ("Layer_Kind " & Layer_Type'Image (Layer.Layer_Kind));
      Put_Line ("Input_Size" & Layer_Range'Image (Layer.Input_Size));
      if Layer.Layer_Kind = Dense_Layer then
         Print_Matrix_Dimensions ("Weights Size", Real_Float_Matrix
                                  (Layer.Weights));
         Put_Line ("Bias Size" & Layer_Range'Image (Layer.Bias'Length));
      end if;

   end Print_Layer_Data;

   --  -------------------------------------------------------------------------

   procedure Print_Network_Data (Name : String; Network : Network_Data) is
   begin
      Put_Line (Name);
      for index in Network.Layers.First_Index ..
        Network.Layers.Last_Index loop
         Put_Line ("Layer" & Integer'Image (index));
         Put_Line ("Layer_Kind " & Layer_Type'Image
                   (Network.Layers.Element (index).Layer_Kind));
         Put_Line ("Input_Size" & Layer_Range'Image
                   (Network.Layers (index).Input_Size));
         if Network.Layers.Element (index).Layer_Kind = Dense_Layer then
            Print_Matrix_Dimensions
              ("Weights Size", Real_Float_Matrix
                 (Network.Layers.Element (index).Weights));
            Put_Line ("Bias Size" & Layer_Range'Image
                      (Network.Layers (index).Bias'Length));
         end if;
      end loop;

   end Print_Network_Data;

   --  -------------------------------------------------------------------------

   procedure Train
     (Network       : in out Network_Data; X_Train : Real_Float_Matrix;
      Y_Train       : Binary_Matrix; Epochs : Positive := 1000;
      Learning_Rate : Float := 0.01; Verbose : Boolean := True) is
      Routine_Name : constant String := "Network.Train ";
      Y_Row        : Binary_Array (Y_Train'Range (2));
      Output_Data  : Real_Float_List;
      Grad         : Real_Float_List;
      Error        : Float;
   begin
      for count in 1 .. Epochs loop
         if count mod 40 = 0 then
            Put ("*");
         end if;

         Error := 0.0;
         for sample in X_Train'Range loop
            --  forward propagate
            Output_Data := To_Real_Float_List (Get_Row (X_Train, sample));
            Predict (Network, Output_Data);
            Y_Row := Get_Row (Y_Train, sample);
            Error := Error + Losses.MSE (Y_Row, Output_Data);
            Grad := To_Real_Float_List (Losses.MSE_Prime (Y_Row, Output_Data));

            for layer in reverse Network.Layers.First_Index ..
              Network.Layers.Last_Index loop
               Backward (Network.Layers (layer), Grad, Learning_Rate);
            end loop;

         end loop;  --  Sample

         --  report mean loss over Epochs
         if Verbose and then
           (Epochs < 10 or else count mod (Epochs / 10) = 1) then
            New_Line;
            Put_Line (Routine_Name & "Epoch" & Integer'Image (count) &
                        " mean error: " &
                        Float'Image (Error / Float (X_Train'Length)));
         end if;
      end loop;  --  Epochs
      New_Line;

   end Train;

   --  -------------------------------------------------------------------------

end Network;
