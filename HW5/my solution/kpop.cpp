#include "kpop.h"
#include "metal.h"
#include "rock.h"
#include "jazz.h"

int KPopBand::play(MusicBand *other)
{
    double score = (this->get_fan_count() + (0.1*this->get_talent()*this->get_energy()));
    this->set_energy(this->get_energy() - (this->get_energy()*0.2));
    
    if (dynamic_cast<KPopBand*>(other)) score *= 2;
    else score *= 0.5;
    
    return ((int) score);
}

void KPopBand::rehearse(void) 
{
    this->set_energy(this->get_energy() - (this->get_energy()*0.5*0.2));
}