#include "tournament_round.h"

// TournamentRound functions goes here

TournamentRound::TournamentRound() { }
TournamentRound::TournamentRound(std::list<MusicBand*>_bands) { bands = _bands ;
}
TournamentRound::TournamentRound(std::vector<MusicBand*>_bands) { 
    bands.assign(_bands.begin(), _bands.end());
}

std::size_t TournamentRound::size() { return bands.size(); }
    
//TournamentRound::TournamentRound(TournamentRound& other) { }
//TournamentRound::TournamentRound(TournamentRound&& other) { }
TournamentRound& TournamentRound::operator=(TournamentRound&& other) {
    if (this != &other) bands = std::move(other.bands);
    return *this; 
}


TournamentRound& TournamentRound::get_next_round() {
    
    std::list<MusicBand*> final;
    MusicBand *b1, *b2;
    int s1, s2;
    
    
    while (bands.size()) {
        
        b1 = bands.front();
        b2 = bands.back();
        
        if (b1 == b2) { // if last element of odd list left
            final.push_back(b1);
            bands.pop_front();
            break;
        }
        
        bands.pop_front();
        bands.pop_back();
        
        if (b1->get_energy() < 0) { // if energy of band 1 is 0 or neg
            b2->set_fan_count(b2->get_fan_count() + b1->get_fan_count());
            b1->set_fan_count(0);
            final.push_back(b2);
            continue;
        }
        
        if (b2->get_energy() < 0) { // if energy of band 2 is 0 or neg
            b1->set_fan_count(b1->get_fan_count() + b2->get_fan_count());
            b2->set_fan_count(0);
            final.push_back(b1);
            continue;
        }
        
        //score checking
        
        s1 = b1->play(b2);
        s2 = b2->play(b1);
        
        if (s1 == s2) {
            final.push_back(b1);
            continue;
        }
        
        if (s1 > s2) s1 = s1 - s2;
        else {
            std::swap(b1,b2);
            s1 = s2 - s1;
        }
        // winner will be put in b1, loser in b2
        
        
        // fan changing
        if (s1 > b2->get_fan_count()) s1 = b2->get_fan_count();
        
        b1->set_fan_count(b1->get_fan_count() + s1);
        b2->set_fan_count(b2->get_fan_count() - s1);
        
        final.push_back(b1);
    }
    
    bands = final;
    return *this;
}


std::ostream& operator<< (std::ostream &os, TournamentRound &other) { 
    
    for (auto i:other.bands) {
        if (i == other.bands.front()) os << i->get_name();
        else os  << "\t" << i->get_name();
    }
    return os;
    
}