const sampleData = [
  {
    "year": 1992,
    "Brazil": 2.12,
    "China": 2.94,
    "India": 2.36,
    "Russia": 2.04,
    "SouthAfrica": 0.53,
    "G7": 29.71
  },
  {
    "year": 1993,
    "Brazil": 2.22,
    "China": 3.34,
    "India": 2.47,
    "Russia": 1.86,
    "SouthAfrica": 0.54,
    "G7": 29.95
  },
  {
    "year": 1994,
    "Brazil": 2.32,
    "China": 3.73,
    "India": 2.6,
    "Russia": 1.6,
    "SouthAfrica": 0.55,
    "G7": 30.53
  },
  {
    "year": 1995,
    "Brazil": 2.38,
    "China": 4.08,
    "India": 2.76,
    "Russia": 1.51,
    "SouthAfrica": 0.56,
    "G7": 30.81
  },
  {
    "year": 1996,
    "Brazil": 2.38,
    "China": 4.39,
    "India": 2.9,
    "Russia": 1.43,
    "SouthAfrica": 0.57,
    "G7": 31.04
  },
  {
    "year": 1997,
    "Brazil": 2.43,
    "China": 4.72,
    "India": 2.97,
    "Russia": 1.43,
    "SouthAfrica": 0.57,
    "G7": 31.5
  },
  {
    "year": 1998,
    "Brazil": 2.4,
    "China": 5.03,
    "India": 3.12,
    "Russia": 1.33,
    "SouthAfrica": 0.57,
    "G7": 32.03
  },
  {
    "year": 1999,
    "Brazil": 2.37,
    "China": 5.3,
    "India": 3.33,
    "Russia": 1.39,
    "SouthAfrica": 0.57,
    "G7": 32.36
  },
  {
    "year": 2000,
    "Brazil": 2.4,
    "China": 5.59,
    "India": 3.36,
    "Russia": 1.52,
    "SouthAfrica": 0.58,
    "G7": 32.7
  },
  {
    "year": 2001,
    "Brazil": 2.4,
    "China": 5.98,
    "India": 3.47,
    "Russia": 1.57,
    "SouthAfrica": 0.59,
    "G7": 32.74
  },
  {
    "year": 2002,
    "Brazil": 2.45,
    "China": 6.44,
    "India": 3.56,
    "Russia": 1.66,
    "SouthAfrica": 0.6,
    "G7": 32.89
  },
  {
    "year": 2003,
    "Brazil": 2.42,
    "China": 6.93,
    "India": 3.75,
    "Russia": 1.83,
    "SouthAfrica": 0.61,
    "G7": 32.69
  },
  {
    "year": 2004,
    "Brazil": 2.47,
    "China": 7.38,
    "India": 3.92,
    "Russia": 1.89,
    "SouthAfrica": 0.61,
    "G7": 32.49
  },
  {
    "year": 2005,
    "Brazil": 2.48,
    "China": 7.99,
    "India": 4.11,
    "Russia": 2.06,
    "SouthAfrica": 0.63,
    "G7": 32.22
  },
  {
    "year": 2006,
    "Brazil": 2.47,
    "China": 8.63,
    "India": 4.25,
    "Russia": 2.4,
    "SouthAfrica": 0.63,
    "G7": 31.8
  },
  {
    "year": 2007,
    "Brazil": 2.52,
    "China": 9.48,
    "India": 4.41,
    "Russia": 2.51,
    "SouthAfrica": 0.64,
    "G7": 31.24
  },
  {
    "year": 2008,
    "Brazil": 2.69,
    "China": 10.57,
    "India": 4.62,
    "Russia": 3.03,
    "SouthAfrica": 0.67,
    "G7": 31.92
  },
  {
    "year": 2009,
    "Brazil": 2.69,
    "China": 11.55,
    "India": 4.98,
    "Russia": 2.89,
    "SouthAfrica": 0.66,
    "G7": 30.88
  },
  {
    "year": 2010,
    "Brazil": 2.77,
    "China": 12.23,
    "India": 5.17,
    "Russia": 2.89,
    "SouthAfrica": 0.65,
    "G7": 30.46
  },
  {
    "year": 2011,
    "Brazil": 2.78,
    "China": 12.94,
    "India": 5.25,
    "Russia": 3.05,
    "SouthAfrica": 0.65,
    "G7": 29.95
  },
  {
    "year": 2012,
    "Brazil": 2.67,
    "China": 13.58,
    "India": 5.5,
    "Russia": 3.11,
    "SouthAfrica": 0.62,
    "G7": 29.55
  },
  {
    "year": 2013,
    "Brazil": 2.81,
    "China": 14.73,
    "India": 5.85,
    "Russia": 3.37,
    "SouthAfrica": 0.66,
    "G7": 30.99
  },
  {
    "year": 2014,
    "Brazil": 2.75,
    "China": 15.13,
    "India": 5.92,
    "Russia": 3.27,
    "SouthAfrica": 0.64,
    "G7": 30.91
  },
  {
    "year": 2015,
    "Brazil": 2.54,
    "China": 15.49,
    "India": 6.12,
    "Russia": 3,
    "SouthAfrica": 0.65,
    "G7": 31.21
  },
  {
    "year": 2016,
    "Brazil": 2.38,
    "China": 15.75,
    "India": 6.37,
    "Russia": 2.89,
    "SouthAfrica": 0.63,
    "G7": 31.06
  },
  {
    "year": 2017,
    "Brazil": 2.31,
    "China": 15.99,
    "India": 6.49,
    "Russia": 2.96,
    "SouthAfrica": 0.61,
    "G7": 30.73
  },
  {
    "year": 2018,
    "Brazil": 2.33,
    "China": 16.42,
    "India": 6.75,
    "Russia": 3.1,
    "SouthAfrica": 0.57,
    "G7": 30.2
  },
  {
    "year": 2019,
    "Brazil": 2.31,
    "China": 16.84,
    "India": 6.88,
    "Russia": 3.17,
    "SouthAfrica": 0.55,
    "G7": 29.95
  },
  {
    "year": 2020,
    "Brazil": 2.34,
    "China": 17.56,
    "India": 6.8,
    "Russia": 3.24,
    "SouthAfrica": 0.53,
    "G7": 29.59
  },
  {
    "year": 2021,
    "Brazil": 2.37,
    "China": 18,
    "India": 7.11,
    "Russia": 3.58,
    "SouthAfrica": 0.53,
    "G7": 29.12
  },
  {
    "year": 2022,
    "Brazil": 2.37,
    "China": 18.01,
    "India": 7.39,
    "Russia": 3.41,
    "SouthAfrica": 0.52,
    "G7": 28.73
  },
  {
    "year": 2023,
    "Brazil": 2.36,
    "China": 18.36,
    "India": 7.7,
    "Russia": 3.42,
    "SouthAfrica": 0.51,
    "G7": 28.4
  }
];