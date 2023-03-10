#pragma once

#define INIT_SEQNO_SERVER 4321
#define INIT_SEQNO_CLIENT 12345

#define RETRANSMISSION_TIMEOUT 0.5
#define KEEPALIVE_TIMEOUT      10
#define FIN_TIMEOUT            2

#define MAX_SEQNO     102400
#define RWND          51200
#define INIT_CWND     512
#define MAX_CWND      51200
#define INIT_SSTHRESH 10000
