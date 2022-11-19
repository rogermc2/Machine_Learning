
with GNATCOLL.Scripts; use GNATCOLL.Scripts;

with ML_Types;

package Aux_Utils is

   procedure Handler (Data : in out Callback_Data'Class; Cmd : String);
   function Load_Data (File_Name : String) return ML_Types.Unbounded_List;

end Aux_Utils;
