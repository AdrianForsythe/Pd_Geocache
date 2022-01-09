#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include "r250.h" 
#define OPTIONS "m:r:p"

typedef struct DATA{
  char    name[30]; //township name  
  double  x,y;      // the center of the township
  double  obs;      // time when rabies was observed 
  double  hum;      // human population density 
  int     edge;     // the eastern edge 
  int     neybors;  // the number of neighbors
  int     *state;   // a vector of states
  int     *adj; 
}DATA;

typedef struct SIMULATION{
  char      modType, modName[30]; 
  int       nReps, r; 
  int       nPars;
  double    *LB, *UB, *params, *best; 
	char      **names;
  double    **events;
  double    **rates;
  double    *simRate; 
	int       *state; 
	double    *avg; 
	double    totChiSq, bestChiSq; 
	bool      putnam; 
}SIMULATION; 

typedef struct MODEL{
  int         nTowns;  
  int         maxT; 
  DATA        *data;
  SIMULATION  *sim;
}MODEL;

//Setup 
MODEL *setupModel(int , char **);
void setupData(MODEL *mod);
void setupSim(int argc, char **argv, MODEL *mod);
void GetOptions(int , char **, MODEL *);
void getData(MODEL *);
void ReadAdjacencyMatrix(MODEL *);
void ReadParams(MODEL *);
void ComputeRates(MODEL *mod);
//Simulate
void Simulate(MODEL *);
double UpdateRates(MODEL *mod);
double ForceEdges(double time, MODEL *mod);
int PickEvent(int N, double tr, double *rt);
double ComputeElapsedTime(double totrate);
double GetRate(int i, int j, MODEL *mod);
double min(double x, double y);
double max(double x, double y);
void Summarize(MODEL *mod);
void Output(MODEL *mod);
void cleanup(MODEL *mod);
void oldParams(MODEL *mod);
void newParams(MODEL *mod);
void showParams(MODEL *mod);
void Maximize(MODEL *mod);

int main(int argc, char **argv){
  MODEL *mod;
  mod = setupModel(argc, argv); 
  Simulate(mod);
	Summarize(mod);
	//Maximize(mod);
	Output(mod);
	cleanup(mod);
}

MODEL *setupModel(int argc, char **argv){
	MODEL *mod;
	//cout << "BEGIN - setupModel \n";
	mod = new MODEL;
  mod->nTowns = 169;
  mod->maxT = 50;
	setupData(mod); 
	setupSim(argc, argv, mod);
  r250_init_t();
	//cout << "END - setupModel \n";
	return mod; 
}

void GetOptions(int argc, char **argv, MODEL *mod){
  char c;
 
	//cout << "BEGIN - GetOptions\n";
  while((c = getopt(argc, argv, OPTIONS)) != -1)
    switch(c){
      case 'm':
        mod->sim->modType = optarg[0]; 
      break;
      case 'r':
        mod->sim->nReps = atoi(optarg);  
      break;
      case 'p':
				mod->sim->putnam = true;
      break;
      default:
      break;
  }
	//cout << "END - GetOptions\n";
}

void setupData(MODEL *mod){
  int i, east[11] = {117,121,124,67,126,90,33,160,89,134,56};
	//cout << "BEGIN - setupData\n";
	mod->data = new DATA[mod->nTowns];
  for (i=0;i<mod->nTowns;i++){
    mod ->data[i].adj = new int[mod->nTowns]; 
    mod ->data[i].edge = 0;  
  }
  for (i=0;i<11;i++) mod->data[east[i]].edge = 1; 
  getData(mod);
  ReadAdjacencyMatrix(mod);
	//cout << "END - setupData\n";
}

void setupSim(int argc, char **argv, MODEL *mod){
	int i;
	//cout << "BEGIN - setupSim\n";
  mod->sim = new SIMULATION;
	mod->sim->nReps = 2; 
	mod->sim->modType = '0'; 
	mod->sim->putnam = false;
	GetOptions(argc, argv, mod);
 	mod->sim->rates = &(new (double *))[mod->nTowns]; 
 	mod->sim->events = &(new (double *))[mod->sim->nReps]; 
 	mod->sim->simRate = new double[mod->nTowns]; 
	mod->sim->avg = new double[mod->nTowns]; 
 	mod->sim->state = new int[mod->nTowns]; 
	for(i=0;i<mod->nTowns;i++)
 		mod->sim->rates[i] = new double[mod->nTowns]; 
	for(i=0;i<mod->sim->nReps;i++)
 		mod->sim->events[i] = new double[mod->nTowns];
  ReadParams(mod);
  ComputeRates(mod);
	//cout << "END - setupSim\n";
}

void getData(MODEL *mod){
  int i, j, tmpd;
  FILE *datafile;
  double tmplf;
  char name[50];
  datafile = fopen ("first.txt", "r");
  fscanf(datafile, "%*s %*s %*s %*s %*s");
  for (i = 0; i < mod->nTowns; i++){
    fscanf (datafile, "%s ", *name); 
    sprintf(mod->data[i].name, "%s ", name);
    fscanf (datafile, "%lf ", &mod->data[i].x); 
    fscanf (datafile, "%lf ", &mod->data[i].y); 
    fscanf (datafile, "%lf ", &mod->data[i].obs); 
    fscanf (datafile, "%lf ", &mod->data[i].hum); 
		mod->data[i].hum = log(mod->data[i].hum); 
  }
  fclose(datafile);
}

void ReadAdjacencyMatrix(MODEL *mod){
  int i,j, tmp;
  FILE *datafile;
  datafile = fopen ("adjacency.txt", "r"); 
  for (i = 0; i < mod->nTowns; i++){
    for (j = 0; j <mod-> nTowns; j++){
      fscanf(datafile, "%d ", &mod->data[i].adj[j]);
			if (mod->data[i].adj[j] > 0)
				mod->data[i].neybors ++;
			else 
				mod->data[i].adj[j] = 0;
		}
	}
  fclose(datafile);
}

void Maximize(MODEL *mod){
	int i;
	for (i=1;i< 3; i=-i){
  	Simulate(mod);
		Summarize(mod);
		newParams(mod);
  	Simulate(mod);
		Summarize(mod);
		if (mod->sim->bestChiSq < mod->sim->totChiSq)
			oldParams(mod);
		showParams(mod);
		}	
}

void showParams(MODEL *mod){
	int i;
	std::cout << "ChiSq = " << mod->sim->totChiSq << std::endl;
	for (i=0;i<mod->sim->nPars;i++)
	  std::cout << mod->sim->names[i] << " " << mod->sim->params[i] << std::endl ; 
	std::cout << std::endl;
}

void oldParams(MODEL *mod){
	int i;
	for (i=0;i<mod->sim->nPars;i++)
		mod->sim->params[i] = mod->sim->best[i]; 
	mod->sim->totChiSq = mod->sim->bestChiSq;
	ComputeRates(mod);
}

void newParams(MODEL *mod){
	int i;
	double nby; 
	//cout << "BEGIN - newParams\n";
	for (i=0;i<mod->sim->nPars;i++){
		//cout << "i = " << i << endl;
		mod->sim->best[i] = mod->sim->params[i]; 
		mod->sim->bestChiSq = mod->sim->totChiSq;
		nby = .02*(dr250() - 0.5)*mod->sim->params[i]; 
		mod -> sim->params[i] += nby;  
	}
	//cout << "END - newParams\n";
	ComputeRates(mod);
}

void Simulate(MODEL *mod){
  int i, j; 
  double time, elapsed, newtime, totrate; 

	for(mod->sim->r=0;mod->sim->r<mod->sim->nReps;mod->sim->r++){
		//cout << "Simulating: " << mod->sim->r << "th rep" << endl; 
	  for(i=0;i<mod->nTowns;i++) mod->sim->state[i] = 1;
	  time = ForceEdges(10, mod);
	  totrate = UpdateRates(mod);
	  do{
			//cout << "time = " << time << endl; 
			//cout << "totrate = " << totrate << endl; 
	    elapsed = ComputeElapsedTime(totrate); 
	    newtime = ForceEdges(time + elapsed, mod);
	    if (newtime > 0)
	      time = newtime;  
	    else{
				do{
	      	j = PickEvent(mod->nTowns, totrate, mod->sim->simRate);
				}while (mod->data[j].edge == 1);
				//cout << "simulated = " << mod->data[j].name <<endl; 
	      time += elapsed; 
	      mod->sim->state[j] = 0;
	      mod->sim->events[mod->sim->r][j] = time;
	    }
	    totrate = UpdateRates(mod);
	  }while(totrate >0); 
	}
}

void ReadParams(MODEL *mod){
  int i;
  FILE *infil;
  char filename[50]; 
  char tmps[20];

	//cout << "BEGIN - ReadParams\n";
  sprintf(filename, "mod%c.in", mod->sim->modType); 
  infil = fopen (filename, "r");
	//cout << "opened " << filename << "\n";
  fscanf(infil, "%s %d ", *mod->sim->modName, &mod->sim->nPars); 
	//cout << "nPars = " << mod->sim->nPars<< "\n";
  mod->sim->LB = new double[mod->sim->nPars];
  mod->sim->UB = new double[mod->sim->nPars];
  mod->sim->params = new double[mod->sim->nPars];
  mod->sim->best = new double[mod->sim->nPars];
  mod->sim->names = &(new (char *))[mod->sim->nPars];
  for(i=0;i<mod->sim->nPars;i++){
    mod->sim->names[i] = new char[20];
    fscanf(infil, "%s %lf %lf %lf", &tmps, 
			&mod->sim->params[i], &mod->sim->LB[i], &mod->sim->UB[i]); 
    sprintf(mod->sim->names[i], "%s", tmps);
  }
  fclose(infil);
	//cout << "END - ReadParams\n";
}

double ForceEdges(double time, MODEL *mod){
  int i;
  double newtime(-1); 

  for (i =0;i<mod->nTowns;i++){
    if ((mod->data[i].edge == 1) && (mod->sim->state[i] == 1) 
      && (time > mod->data[i].obs)){
          if (newtime == -1)
            newtime = mod->data[i].obs; 
          else 
            newtime = min(mod->data[i].obs, newtime); 
    }
  }
  if (newtime > -1){ 
    for (i=0;i<mod->nTowns;i++){
      if ((mod->data[i].edge == 1) && (mod->sim->state[i] == 1) 
        && (mod->data[i].obs == newtime)){
        mod->sim->state[i] = 0;
        mod->sim->events[mod->sim->r][i] = newtime; 
				//cout << "forced = " << mod->data[i].name <<endl; 
      }
    }
  }
  return newtime;
}

int PickEvent(int N, double tr, double *rt){
  int i;
  double index; 

  index = dr250()*tr;
  i = -1;
  do{
    i++;
    index -= rt[i];
  }while(index>0);
  return i;
}

double ComputeElapsedTime(double totrate){
  return -log(1-dr250())/totrate; 
}

void ComputeRates(MODEL *mod){
  int i, j;
  for (i=0;i<mod->nTowns; i++) 
    for (j=0;j<mod->nTowns; j++)
      mod->sim->rates[i][j] = GetRate(i,j, mod); 
}

double GetRate(int i, int j, MODEL *mod){
	double rate, alpha, beta, gamma, rho, mu; 

	//rate of spread to i from j 
  if (i == j) {
    rate = mod->sim->params[0];
    switch(mod->sim->modType){
       case '4': //RivHum2
        rate += mod->sim->params[1]*mod->data[i].hum;
      break;
      default:
      break;
    }
  }
  else if (mod->data[i].adj[j] == 0){ 
		rate = 0;
	}
  else { 
    switch(mod->sim->modType){
      case '0': // Null 
        rate =  mod->sim->params[1];  
      break;
      case '1':   // humans
        rate = mod->sim->params[1] + 
					+ mod->sim->params[2]*mod->data[j].hum;
      break;
      case '2':   // river
				if (mod->data[i].adj[j] == 1)
        	rate =  mod->sim->params[1];  
				else 
        	rate =  mod->sim->params[2];  
      break;
      case '3':  // river, humans
				if (mod->data[i].adj[j] == 1)
        	rate = mod->sim->params[1] + 
						+ mod->sim->params[2]*mod->data[j].hum;
				else 
        	rate = mod->sim->params[3] + 
						+ mod->sim->params[4]*mod->data[j].hum;
      break;
      case '4':  // RivHum2 
				if (mod->data[i].adj[j] == 1)
        	rate =  mod->sim->params[2];  
				else 
        	rate =  mod->sim->params[3];  
      break;
      default:
        std::cout << "BAD MODEL SUBTYPE\n";
        exit(33);
      break;
    }
    rate = rate/(double)mod->data[i].neybors;
  }
  if(rate < 0) rate = 0.0; 
  return rate;
}


double UpdateRates(MODEL *mod){
	int i,j;
	double trate(0);
	for(i=0;i<mod->nTowns;i++){
		mod->sim->simRate[i] = 0;
		if(mod->sim->state[i] == 1){
			mod->sim->simRate[i] = mod->sim->rates[i][i]; 
			for(j=0;j<mod->nTowns;j++){
				if((mod->sim->state[j] == 0)  && (mod->data[i].adj[j] > 0))
					mod->sim->simRate[i] += mod->sim->rates[i][j]; 
				}
			trate += mod->sim->simRate[i];
		}
	}
	return trate;
}

double max(double x, double y){
	if (x > y) return x; 
	else return y;
}

double min(double x, double y){
	if (x < y) return x; 
	else return y;
}

void Summarize (MODEL *mod){
	int i,j; 
	//cout << "BEGIN - Summarize \n";
	mod->sim->totChiSq = 0;
	for (i=0;i<mod->nTowns;i++){
		mod->sim->avg[i] = 0; 
		for (j=0;j<mod->sim->nReps;j++){
			mod->sim->avg[i] += mod->sim->events[j][i];
		}
		mod->sim->avg[i] = mod->sim->avg[i]/(double)mod->sim->nReps;
		if ((i == 115) && (mod->sim->putnam == false)){
			std::cout << "Ignoring Putnam! "; 
		}
		else{
			mod->sim->totChiSq += 
			(mod->sim->avg[i] - mod->data[i].obs)
			*(mod->sim->avg[i] - mod->data[i].obs)
			/(mod->sim->avg[i]);
		}
	}
	//cout << "END - Summarize \n";
}

void Output (MODEL *mod){
	int i; 
	FILE *output;
	output = fopen ("out.txt", "w");
	fprintf(output, "Model: %s\n", mod->sim->modName); 
	fprintf(output, "ChiSq: %lf\n", mod->sim->totChiSq); 
	fprintf(output, "nReps: %d\n", mod->sim->nReps); 
	for (i=0;i<mod->sim->nPars;i++)
		fprintf(output, "%s = %lf\n", mod->sim->names[i], mod->sim->params[i]);
	fprintf(output, "\n"); 
	for (i=0;i<mod->nTowns;i++)
		fprintf(output, "%s %2.0lf %2.2lf\n", mod->data[i].name, 
			mod->data[i].obs, mod->sim->avg[i]);
	fclose(output);
}

void cleanup(MODEL *mod){
	delete mod;
}
