
with Maths;

package body Structure is

   procedure Add_Node (aModel     : in out Model; Node_Size : Positive;
                       Activation : Activation_Kind := Identity_Activation) is
      Level : Node (Node_Size);
   begin
      Level.Activation := Activation;
      aModel.Nodes.Append (Level);

   end Add_Node;

   --  ---------------------------------------------------------------------------

   function Connect (Data : Real_Float_Vector; Level_1 : Node)
                     return Real_Float_Matrix is
      Connection : Real_Float_Matrix (Data'Range, Level_1.Level'Range);
   begin
      --  Initialze connection weights
      for row in Connection'Range loop
         for col in Connection'Range (2) loop
            Connection (row, col) := 1.0;
         end loop;
      end loop;
      return Connection;

   end Connect;

   --  ---------------------------------------------------------------------------

   function Connect (Level_A, Level_B : Node) return Real_Float_Matrix is
      Connection : Real_Float_Matrix (Level_A.Level'Range,
                                      Level_B.Level'Range);
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

   procedure Make_Connections (aModel : in out Model) is
      Connect_Inputs : constant Real_Float_Matrix
        := Connect (aModel.Input_Data, aModel.Nodes.First_Element);
   begin
      aModel.Connect_List.Append (Connect_Inputs);
      for item in aModel.Nodes.First_Index ..
        aModel.Nodes.Last_Index - 1 loop
         declare
            Connect_Nodes : Real_Float_Matrix
              := Connect (aModel.Nodes (item), aModel.Nodes (item + 1));
         begin
            aModel.Connect_List.Append (Connect_Nodes);
         end;
      end loop;

   end Make_Connections;

   --  ---------------------------------------------------------------------------


end Structure;
