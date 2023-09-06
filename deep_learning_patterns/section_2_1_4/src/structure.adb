
with Maths;

with Stochastic_Optimizers;

package body Structure is

   procedure Forward (aModel : in out Sequential_Model; Loss_Method : Loss_Kind);

   --  ---------------------------------------------------------------------------

   procedure Add_Layer (aModel    : in out Sequential_Model; Layer_Size : Positive;
                       Activation : Activation_Kind := Identity_Activation) is
      Level : Layer (Layer_Size);
   begin
      Level.Activation := Activation;
      aModel.Layers.Append (Level);

   end Add_Layer;

   --  ---------------------------------------------------------------------------

   procedure Add_Node (aLayer     : in out Layer; Node_Size : Positive;
                       Activation : Activation_Kind := Identity_Activation) is
      Level : Node (Node_Size);
   begin
      Level.Activation := Activation;
      aLayer.Nodes.Append (Level);

   end Add_Node;

   --  ---------------------------------------------------------------------------

   procedure Back_Propogate (aModel : Sequential_Model; Loss_Method : Loss_Kind) is
   begin
      null;

   end Back_Propogate;

   --  ---------------------------------------------------------------------------

   procedure Compile (aModel : in out Sequential_Model; Loss_Method : Loss_Kind) is
   begin
      Forward (aModel, Loss_Method);

   end Compile;

   --  ---------------------------------------------------------------------------

   function Connect (Data : Real_Float_Vector; Layer_1 : Layer)
                     return Real_Float_Matrix is
      Connection : Real_Float_Matrix (Data'Range, Layer_1.Level'Range);
   begin
      for row in Connection'Range loop
         for col in Connection'Range (2) loop
            Connection (row, col) := 1.0;
         end loop;
      end loop;
      return Connection;

   end Connect;

   --  ---------------------------------------------------------------------------

   function Connect (Layer_A, Layer_B : Layer) return Real_Float_Matrix is
      Connection : Real_Float_Matrix (Layer_A.Level'Range,
                                      Layer_B.Level'Range);
   begin
      --  Initialze connection weights
      for row in Connection'Range loop
         for col in Connection'Range (2) loop
            --  Random_Float generates a random number in the range  -1.0 .. 1.0
            Connection (row, col) := Maths.Random_Float;
         end loop;
      end loop;
      return Connection;

   end Connect;

   --  ---------------------------------------------------------------------------

   procedure Forward (aModel      : in out Sequential_Model;
                      Loss_Method : Loss_Kind) is
      use Real_Float_Arrays;
      use Layer_Packge;
   begin
      for index in aModel.Layers.First_Index .. aModel.Layers.Last_Index - 1 loop
         null;
--           aModel.Nodes (index + 1).Level :=
--             aModel.Nodes (index).Level * aModel.Connect_List (index + 1);
      end loop;

   end Forward;

   --  ---------------------------------------------------------------------------

   procedure Make_Connections (aModel : in out Sequential_Model) is
      use Real_Float_Arrays;
      Connect_Inputs : constant Real_Float_Matrix
        := Connect (aModel.Input_Data, aModel.Layers.First_Element);
   begin
      aModel.Connect_List.Append (Connect_Inputs);
      for item in aModel.Layers.First_Index ..
        aModel.Layers.Last_Index - 1 loop
         declare
            Connect_Nodes : Real_Float_Matrix
              := Connect (aModel.Layers (item), aModel.Layers (item + 1));
         begin
            aModel.Connect_List.Append (Connect_Nodes);
         end;
      end loop;

      aModel.Layers (aModel.Layers.First_Index).Level :=
        aModel.Input_Data *
          aModel.Connect_List (aModel.Connect_List.First_Index);

   end Make_Connections;

   --  ---------------------------------------------------------------------------

   function Get_Output_Layer (aModel : Sequential_Model) return Layer is
   begin

      return aModel.Layers.Last_Element;

   end Get_Output_Layer;

   --  ---------------------------------------------------------------------------


end Structure;
