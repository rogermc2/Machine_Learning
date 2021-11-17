
with Config;
with Export_Types; use Export_Types;

package Export_Utilities is

   procedure Get_Name (aLine   : in out Lines.Bounded_String;
                       Finish  : in out Natural;
                       N       : out Config.Name);
   function Pad (S : in String) return Config.Name;

end Export_Utilities;
