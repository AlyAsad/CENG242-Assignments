#include "jazz.h"
#include "kpop.h"
#include "metal.h"
#include "rock.h"


int JazzBand::play(MusicBand *other)
{
    double score = (this->get_fan_count() + (0.1*this->get_talent()*this->get_energy()));
    this->set_energy(this->get_energy() - (this->get_energy()*0.06));
    
    if (dynamic_cast<KPopBand*>(other)) score *= 0.5;
    else if (dynamic_cast<MetalBand*>(other)) score *= 1.3;
    else if (dynamic_cast<RockBand*>(other)) score *= 0.7;
    else if (dynamic_cast<JazzBand*>(other)) score *= 0.7;
    
    return ((int) score);
}

void JazzBand::rehearse(void) 
{
    this->set_energy(this->get_energy() - (this->get_energy()*0.5*0.06));
    this->set_talent(this->get_talent() + 5);
}