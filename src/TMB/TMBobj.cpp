#define TMB_LIB_INIT R_init_MixME
#include <TMB.hpp>

// find cases of missing values
template<class Type>
bool isNA(Type x){
  return R_IsNA(asDouble(x));
}

// Define class structure for list of vectors
template<class Type>
struct listVector : vector<vector<Type> > {
  listVector() : vector<vector<Type> >() {};
  listVector(int n) : vector<vector<Type> >(n) {};
  listVector(SEXP x){
    (*this).resize(LENGTH(x));
    for(int i =0; i < LENGTH(x); i++) {
      SEXP sm = VECTOR_ELT(x,i);
      (*this)(i) = asVector<Type>(sm);
    }
  }
  template<class T>
  listVector<T> cast() const {
    int n = (*this).size();
    listVector<T> d(n);
    for(int i = 0; i < n; ++i){
      vector<T> tmp = (*this)(i). template cast<T >() ;
      d(i) = tmp ;
    }
    return d;
  }
  listVector<Type>& operator=(const listVector<Type>& rhs ) {
    (*this).resize(rhs.size());
    for(int i = 0; i < rhs.size(); ++i){
      vector<Type> tmp = rhs(i);
      (*this)(i) = tmp;
    }
    return *this ;
  }
};

// Define class structure for a list of matrices
template<class Type>
struct listMatrix : vector<matrix<Type> > {
  
  listMatrix() : vector<matrix<Type> >() {};
  listMatrix(int n) : vector<matrix<Type> >(n) {};
  listMatrix(SEXP x){ 
    (*this).resize(LENGTH(x));
    for(int i=0; i<LENGTH(x); i++){
      SEXP sm = VECTOR_ELT(x, i);
      (*this)(i) = asMatrix<Type>(sm);
    }
  }
  
  template<class T>
  listMatrix<T> cast() const {
    int n = (*this).size();
    listMatrix<T> d(n);
    for(int i = 0; i < n; ++i){
      matrix<T> tmp = (*this)(i).template cast<T>();
      d(i) = tmp;
    }
    return d;
  }
  
  listMatrix<Type>& operator=(const listMatrix<Type>& rhs) {
    (*this).resize(rhs.size());
    for(int i = 0; i < rhs.size(); ++i){
      matrix<Type> tmp = rhs(i);
      (*this)(i) = tmp;
    }
    return *this;
  }
};

template<class Type>
Type objective_function<Type>::operator()(){
  
  // Read in data
  // -------------------------
  
  // Extract data and parameters
  DATA_ARRAY(quota);    // matrix: rows nstocks, cols nfleets
  DATA_ARRAY(catchq);   // matrix: rows nstocks, cols nfleets
  DATA_STRUCT(landwt,   listMatrix);   // list: nstocks (matrix: rows = nages, cols = nfleets)
  DATA_STRUCT(discwt,   listMatrix);   // list: nstocks (matrix: rows = nages, cols = nfleets)
  DATA_STRUCT(landfrac, listMatrix);   // list: nstocks (matrix: rows = nages, cols = nfleets)
  DATA_STRUCT(catchsel, listMatrix);   // list: nstocks (matrix: rows = nages, cols = nfleets)
  DATA_STRUCT(m, listVector);         // list: nstocks (vector: nages)
  DATA_STRUCT(n, listVector);         // list: nstocks (vector: nages)
  DATA_STRING(adviceType); // "catch" or "landings"
  DATA_STRING(objType);    // "global" or "choke"
  DATA_IVECTOR(stkLim);    // vector: nfleets
  
  PARAMETER_VECTOR(logE);
  
  // Exponentiate parameters
  vector<Type> Eff = exp(logE);
  
  // calculate number of stocks and fleets
  int nstk = catchq.dim(0);
  int nflt = catchq.dim(1);
  
  // calculate catch
  // -----------------------------

  // prepare arrays to hold results
  array<Type> partF(nstk, nflt);  // partial F (nstocks, nfleets)
  array<Type> Cfleet(nstk, nflt); // fleet catch (nstocks, nfleets)
  array<Type> quotaResid(nstk, nflt);
  partF.setZero();
  Cfleet.setZero();
  quotaResid.setZero();
  
  // prepare residual quota penalty object
  Type ans = 0;

  // loop over stocks
  for(int s = 0; s < nstk; s++){
    
    // extract stock data
    vector<Type> n_stk = n(s); // stock numbers-at-age
    vector<Type> m_stk = m(s); // stock natural mortality-at-age
    
    // Extract fleet-stock data
    matrix<Type> landwt_s = landwt(s);
    matrix<Type> discwt_s = discwt(s);
    matrix<Type> landfrac_s = landfrac(s);
    matrix<Type> catchsel_s = catchsel(s);
    
    // identify number of ages
    int nage = n_stk.size();
    
    // Initialise vector to hold total F-at-age
    vector<Type> Fage(nage);
    Fage.setZero();
    
    // first loop over fleets to calculate overall stock F-at-age
    for(int f = 0; f < nflt; f++){
      partF(s,f) = catchq(s,f) * Eff(f);
      
      for(int a = 0; a < nage; a++) {
        Fage(a) += partF(s,f) * catchsel_s(a,f);
      }
    } // END loop over fleets
    
    
    // loop over fleets again
    for(int f = 0; f < nflt; f++) {
      
      // prepare vector of partial F-at-age for stock s and fleet f
      vector<Type> partFage(nage);
      partFage.setZero();
      
      // prepare vector of partial L-at-age and D-at-age for stock s and fleet f
      vector<Type> partLage(nage);
      vector<Type> partDage(nage);
      vector<Type> partLWage(nage);
      vector<Type> partDWage(nage);
      partLage.setZero();
      partDage.setZero();
      partLWage.setZero();
      partDWage.setZero();
      
      // loop over each age
      for(int a = 0; a < nage; a++) {
        
        // calculate partial F-at-age
        partFage(a) = partF(s,f) * catchsel_s(a,f);
        
        // Calculate discards fraction
        Type discfrac_s = Type(1) - landfrac_s(a,f);
        
        // calculate corresponding landings and discards number at age
        partLage(a) = (partFage(a)/(Fage(a) + m_stk(a))) * (Type(1)-exp(-(Fage(a) + m_stk(a)))) * n_stk(a) * (landfrac_s(a,f));
        partDage(a) = (partFage(a)/(Fage(a) + m_stk(a))) * (Type(1)-exp(-(Fage(a) + m_stk(a)))) * n_stk(a) * (discfrac_s);
        
        // Extract landings and discards weights
        Type landwt_sf = landwt_s(a,f);
        Type discwt_sf = discwt_s(a,f);
        
        // If NA, then set to zero - Not ideal. I should be catching this before
        // I attempt to optimise
        // if(isNA(landwt_sf)) {
        //   landwt_sf = 0;
        // }
        // if(isNA(discwt_sf)) {
        //   discwt_sf = 0;
        // }
        
        // calculate landings and discards biomass
        partLWage(a) = partLage(a) * landwt_sf;
        partDWage(a) = partDage(a) * discwt_sf;
        
      } // END loop over ages
      
      // if advice type = catch
      if(adviceType == "catch") {
        Cfleet(s,f) = sum(partLWage) + sum(partDWage);
      }

      // if advice type = landings
      if(adviceType == "landings") {
        Cfleet(s,f) = sum(partLWage);
      }
      
      // calculate residual quota
      quotaResid(s, f) = quota(s, f) - Cfleet(s, f);
      
      // if optimising effort to minimise overall over- and under-shoot
      if(objType == "global"){
        
        // The R version of this code has an "if" statement here. I cannot do this
        // in TMB because this prevent differentiation.
        //
        // The solution is to calculate an exponent that is close to 2 below 0 and
        // close to 1 above zero
        
        Type expon = (Type(1)/(Type(1)+exp(quotaResid(s, f))))+Type(1); // calculate exponent
        Type abs_quotaResid = sqrt(pow(quotaResid(s, f), Type(2))); // calculate absolute value
        
        ans += pow(abs_quotaResid, expon);
        
      }
    } // END loop over fleets
  } // END loop over stocks
  
  if(objType == "choke") {
    // Now loop over fleets and extract the catch for the choke-stock for each fleet
    for(int f = 0; f < nflt; f++) {
      
      // extract choke-stock index
      int choke_idx = stkLim(f);
      
      // extract residual quota for choke stock
      Type quotaResid_lim = quotaResid(choke_idx, f);
      ans += pow(quotaResid_lim, 2);
    }
  }

  REPORT(Cfleet);
  // REPORT(partF);
  
  return ans;
}
