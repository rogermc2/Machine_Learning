
with Config;
with Dot_Tables;

package State_Machine is

   procedure Parse_Line (Table : in out Dot_Tables.Table_Data;
                         N : Config.Name);

end State_Machine;
