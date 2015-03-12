#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <jack/jack.h>
#include <jack/ringbuffer.h>
#include <sndfile.h> 
#include <math.h>

jack_port_t *input_port;
jack_port_t *output_port;
jack_client_t *client;

jack_nframes_t delay_index;
jack_nframes_t latency;
jack_default_audio_sample_t *delay_buffer;
jack_nframes_t maxlatency = 48000 * 2;
jack_nframes_t minlatency = 16;
jack_nframes_t extralatency = 256 * 3;

char path[1024];
SF_INFO sf_info;
SNDFILE *sf;

float noise_coef = 0.03, signal_coef = 64;

float frand(){
  return ((float) rand() / (float) RAND_MAX) - 0.5;
}

int process(jack_nframes_t nframes, void *arg){ 
  jack_default_audio_sample_t *in, *out;
  int k;

  out = jack_port_get_buffer(output_port, nframes);
  in = jack_port_get_buffer(input_port, nframes);

  sf_writef_float(sf, in, nframes);
  for(k=0; k<nframes; k++){
    out[k] = delay_buffer[delay_index] * signal_coef + noise_coef * frand(); 
    delay_buffer[delay_index] = in[k];
    delay_index = (delay_index + 1) % latency;
  }
  
  return 0;
}

void setup(int userid){
  const char **ports;
  client = jack_client_open("daf", JackNullOption, NULL);
  delay_index = 0;
  latency = minlatency;
  delay_buffer = malloc(sizeof(jack_default_audio_sample_t) * maxlatency); 
  jack_set_process_callback(client, process, 0);
  input_port = jack_port_register(client, "input", JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0);
  output_port = jack_port_register(client, "output", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
  jack_activate(client);

	ports = jack_get_ports (client, NULL, NULL, JackPortIsPhysical|JackPortIsOutput);
	if (ports == NULL)
		fprintf(stderr, "no physical capture ports\n");
	if (jack_connect (client, ports[0], jack_port_name (input_port))) 
		fprintf (stderr, "cannot connect input ports\n");
	free (ports);
	
	ports = jack_get_ports (client, NULL, NULL, JackPortIsPhysical|JackPortIsInput);
	if (ports == NULL)
		fprintf(stderr, "no physical playback ports\n");
	if (jack_connect (client, jack_port_name (output_port), ports[0]))
		fprintf (stderr, "cannot connect output ports\n");
	if (jack_connect (client, jack_port_name (output_port), ports[1]))
		fprintf (stderr, "cannot connect output ports\n");
	free (ports);
	
  sf_info.samplerate = jack_get_sample_rate (client);
	sf_info.channels = 1;
	
	sf_info.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
 
  sprintf(path, "./data/%d.wav", userid);
  sf = sf_open(path, SFM_WRITE, &sf_info);
}

void jackClose(){
  jack_client_close(client);
}

//Note: not threadsafe, could fail?
void setDelay(float delay){
  latency = ceil(delay * (float)sf_info.samplerate);
  latency = latency > extralatency ? latency - extralatency : latency;
  latency = latency > minlatency ? latency : minlatency;
}
