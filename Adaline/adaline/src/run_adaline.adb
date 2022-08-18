
with Ada.Text_IO; use Ada.Text_IO;

with Connectionist; use Connectionist;
with Connectionist_Types; use Connectionist_Types;
with Training;

procedure Run_Adaline is
   Num_Data             : Natural;
   Num_Layers           : Natural;
   Layer_Entries        : Layer_List;
   Active_Layer         : Boolean;
   Record_Weights       : Boolean := False;
   Write_Error          : Boolean := False;
   Key                  : Character;
   Finish               : Boolean := False;
   Pattern_Active       : Boolean := False;
   Output_Data          : Output_Type
     := Training.Initialize_Training (Num_Data, Num_Layers, Layer_Entries,
                                      Active_Layer);
   Num_Weights          : Natural := Num_Layers;
begin

   declare
      Neuron       : Processing_Element (Num_Data, Num_Weights);
   begin
   while not Finish loop
      New_Line;
      Put_Line ("Main Menu");
      Put_Line ("1: train neuron");
      Put_Line ("2: test the neuron");
      Put_Line ("3: save neuron weights to file");
      Put_Line ("4: choose training patterns");
      Put_Line ("5: watch pattern de training actual");
      Put_Line ("6: change learning speed ");
      Put_Line ("7: change maximimun permitted error ");
      Put_Line ("8: record EMC successive to file");
      Put_Line ("9: record successive weights to file");
      Put_Line ("0: exit");

      Put ("Enter option: ");
      Get (Key);
      case Key is
         when '1' => Training.Train_Neuron
              (Neuron, Num_Data, Num_Layers, Active_Layer, Record_Weights,
               Write_Error);
         when '2' => null; --  testneuron;
         when '3' => null; --  saveweights;
         when '4' => null;
         when '5' => null; --  verpatrentr;
         when '6' => null; --  introvelalearn;
         when '7' => null; --  intromaxerror;
         when '8' => null; --  recorderror:=not(recorderror);
         when '9' => null; --  recordweights:=not(recordweights);
         when '0' => Finish := True;
         when others =>
            Put_Line ("Invalid option!");
      end case;

      end loop;
      end;  --  declare block

end Run_Adaline;

--  ---------------------------------------------------------------------------------------------
