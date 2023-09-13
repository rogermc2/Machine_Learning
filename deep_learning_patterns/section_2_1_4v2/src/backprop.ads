with Base_Neural;
with Basic_Printing; use Basic_Printing;
with Stochastic_Optimizers;
with Structure_V2; use Structure_V2;

package Backprop is

   function Back_Propogate (aModel      : Sequential_Model;
                             Optimiser  : Stochastic_Optimizers.Optimizer_Record;
                            Loss_Method : Loss_Kind)
                            return Stochastic_Optimizers.Parameters_List;
end Backprop;
