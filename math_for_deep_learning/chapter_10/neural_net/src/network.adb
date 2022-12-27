
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;
--  with Neural_Utilities;
--  with NL_Types;

package body Network is

   --     Neural_Network  : Network_Package.Map;

   procedure Add (Network : in out Network_Data; Layer : Layer_Data) is
   begin
      Network.Layers.Append (Layer);
   end Add;

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
