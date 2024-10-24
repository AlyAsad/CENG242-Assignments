#include "metal.h"
#include "kpop.h"
#include "rock.h"
#include "jazz.h"

int MetalBand::play(MusicBand *other)
{
    double score = (this->get_fan_count() + (0.1*this->get_talent()*this->get_energy()));
    this->set_energy(this->get_energy() - (this->get_energy()*0.16));
    
    if (dynamic_cast<KPopBand*>(other)) score *= 0.5;
    else if (dynamic_cast<MetalBand*>(other)) score *= 1.0;
    else if (dynamic_cast<RockBand*>(other)) score *= 1.5;
    else if (dynamic_cast<JazzBand*>(other)) score *= 1.1;
    
    return ((int) score);
}

void MetalBand::rehearse(void) 
{
    this->set_energy(this->get_energy() - (this->get_energy()*0.5*0.16));
    this->set_talent(this->get_talent() - 5);
}