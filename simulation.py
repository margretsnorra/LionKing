import argparse
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from collections import namedtuple

# Define the named tuple
IncomingProbType = namedtuple('IncomingProbType', ['type', 'probability'])
# Create an enum for the event types
Event = namedtuple('Event', ['type', 'simtime', 'commontype'])


def read_local_file(csv_file):
    return pd.read_csv(csv_file, index_col=0)

def convert_minutes_to_seconds(mean_values):
    """Convert mean values from minutes to seconds."""
    for event_type, mean_time in mean_values.items():
        mean_values[event_type] = mean_time * 60
    return mean_values


def plot_event_histograms(events):
    fig, axs = plt.subplots(int(np.ceil(len(events) / 2)), 2, figsize=(10, 15))
    axs = axs.flatten()
    for i, (event, times) in enumerate(events.items()):
        axs[i].hist(np.array(times), bins=10, edgecolor='black')
        axs[i].set_title(f'Histogram of {event}')
        axs[i].set_xlabel('Duration')
        axs[i].set_ylabel('Frequency')
    plt.tight_layout()
    plt.savefig('histograms.png')


def incoming_event_odds(events, incoming_events=['A', 'B', 'C', 'D']):
    """ Calculate the odds of incoming events """
    incoming_freq = [IncomingProbType('A', 0.07),
                     IncomingProbType('B', 0.15),
                     IncomingProbType('C', 0.35),
                     IncomingProbType('D', 0.43)]
    assert sum([i.probability for i in incoming_freq]) == 1, "Incoming frequencies must sum to 1"
    return incoming_freq


def expon(mean):
    """ Random number generator for exponential distribution """
    return -mean * np.log(np.random.uniform())


@staticmethod
def sort_list(list, field):
    """ Sort a list of named tuples by a field """
    return sorted(list, key=lambda x: getattr(x, field))


class Simulation:
    raise NotImplementedError("Function not yet implemented")
    def __init__(self, mean_values, incoming_prob, event_types, workstations):
        """ Simulate the ÖSSUR factory, given the mean values and incoming probabilities for a maximum time.

        Args:
        - mean_values (dict): Dictionary of mean values for each event type
        - incoming_prob (list): List of incoming probabilities for each event type
        """
        assert sum([i.probability for i in incoming_prob]) == 1, "Incoming probabilities must sum to 1"
        assert all([v > 0 for v in mean_values.values()]), "All mean values must be positive"
        self.mean_values = mean_values
        self.incoming_prob = incoming_prob
        self.event_types = event_types
        self.workstations = workstations #ATH
        
        # Initialize the simulation
        # ATH ATH ATH
        self.event_list = []
        self.queue = {key: 0 for key in self.event_types}
        self.workstation = {key: 0 for key in self.event_types}
        self.restart()

    def restart(self):
        """ Restart the simulation """
        self.simtime = 0
        self.last_simtime = 0
        self.stats = {'products': 0,
                      'queue': {key: 0 for key in self.event_types},
                      'workstation': {key: 0 for key in self.event_types}
                      }

    def timing(self):
        """ Set the simulation time to the next event time and return the next event type

        Returns:
            - next_event (Event): Next event type and simtime
        """
        self.last_simtime = self.simtime
        # The first event out of the event list, using pop(0)
        next_event = self.event_list.pop(0)  # This removes the first element from the list as well as returning it
        self.simtime = next_event.simtime
        return next_event

    def event_schedule(self, event):
        """ Schedule a new event

        Args:        
        - event (Event): Event type and time
        """
        self.event_list.append(event)  # Add the new event to the event list
        self.event_list[:] = sort_list(self.event_list, 'simtime')  # Replace the event list with a sorted list

    def new_product(self):
        """ New product arrival. Choose a random type of product and schedule the next arrival """
        u = np.random.uniform()

        cumulative_probability = 0
        for prob in self.incoming_prob:
            cumulative_probability += prob.probability
            if u < cumulative_probability:
                new_type = prob.type
                break

        # Schedule the next arrival
        self.event_schedule(Event(new_type + "Inn", self.simtime + 0, new_type))

        # Schedule a new product arrival
        self.event_schedule(Event('Koma', self.simtime + expon(self.mean_values['Koma']), 'Koma'))

    def update_stats(self):
        """ Update the statistics in workstations and queues """
        for event_type in self.event_types:
            self.stats['queue'][event_type] += (self.simtime - self.last_simtime) * self.queue[event_type]
            self.stats['workstation'][event_type] += (self.simtime - self.last_simtime) * self.workstation[event_type]

    def arrive(self, event):
        """
        product arrival at an event. Update the statistics.
        If the workstation is occupied, increase the queue. If not, occupy the workstation and schedule the departure.

        Args:
        - sim (SimulationGlobalVars): Simulation global variables
        - event (Event): Event type and simtime

        Returns:
        - Updated simulation global variables
        """
        event_type = event.commontype
        # Is the workstation occupied?
        if self.workstation[event_type] > 0:
            # Yes, increase the queue
            self.queue[event_type] += 1
        else:
            # No, occupy the workstation
            self.workstation[event_type] = 1
            # Schedule the departure
            self.event_schedule(
                Event(event_type + 'Ut', self.simtime + expon(self.mean_values[event_type]), event_type))

    def depart(self, event):
        """
        Departure from an event. Update the statistics. Clear the workstation and schedule the next arrival (if queued).
        
        Args:
        - event (Event): Event type and simtime
        """
        event_type = event.commontype

        # Send new product to the workstation from the queue
        if self.queue[event_type] > 0:
            self.event_schedule(Event(event_type + 'Inn', self.simtime + 0, event_type))
            self.queue[event_type] -= 1

        if event_type in ['Kassi1', 'Kassi2']:
            # Final station, product leaves
            self.stats['products'] += 1
        else:
            # Schedule next station
            if event_type in ['Matur', 'Samloka']:
                self.event_schedule(Event('DrykkurInn', self.simtime + 0, 'Drykkur'))
            elif event_type == 'Drykkur':
                if self.queue['Kassi1'] < self.queue['Kassi2']:
                    register = 'Kassi1'
                elif self.queue['Kassi1'] > self.queue['Kassi2']:
                    register = 'Kassi2'
                else:
                    register = np.random.choice(['Kassi1', 'Kassi2'])
                self.event_schedule(Event(register + 'Inn', self.simtime + 0, register))

        self.workstation[event_type] = 0

    def simulate(self, max_time):
        self.event_schedule(Event('Koma', self.simtime + expon(self.mean_values['Koma']), 'Koma'))

        while len(self.event_list) > 0 and self.simtime < max_time:
            next_event = self.timing()
            self.update_stats()
            if next_event.type == "Koma":
                self.new_product()
            elif next_event.type.endswith('Inn'):
                self.arrive(next_event)
            elif next_event.type.endswith('Ut'):
                self.depart(next_event)

        self.stats['queue'] = {k: v / self.simtime for k, v in self.stats['queue'].items()}
        self.stats['workstation'] = {k: v / self.simtime for k, v in self.stats['workstation'].items()}
        print(f"\tSimulation finished at time {self.simtime} seconds")
        print(f'\tProducts: {self.stats["products"]}')
        print(f'\tQueue: {self.stats["queue"]}')
        print(f'\tWorkstation: {self.stats["workstation"]}')


def process_stats(stats, event_types):
    """ Process the simulation statistics """
    print(f"\nSimulation statistics: {len(stats)} runs")
    print(
        f"products: {np.mean([s['products'] for s in stats]):.2f} +/- {np.std([s['products'] for s in stats]):.2f}")
    for event_type in event_types:
        print(
            f"{event_type} Queue: {np.mean([s['queue'][event_type] for s in stats]):.2f} +/- {np.std([s['queue'][event_type] for s in stats]):.2f}")
        print(
            f"{event_type} Workstation: {np.mean([s['workstation'][event_type] for s in stats]):.2f} +/- {np.std([s['workstation'][event_type] for s in stats]):.2f}")


def main():
    parser = argparse.ArgumentParser(description="Run a simulation based on Lego Hama data.")
    parser.add_argument("--csv_file", default='ossur_vitlaust.csv',
                        help="Path to the csv file containing the data (default: %(default)s)"
                        )
    parser.add_argument("--max_time", type=int, default=28800, help="Maximum simulation time")
    parser.add_argument("--runs", type=int, default=100, help="Number of simulation runs")
    parser.add_argument("--warmup", action="store_true", help="Run a warmup period before the simulation")
    args = parser.parse_args()

    mean_values = read_local_file(args.csv_file)
    mean_values = convert_minutes_to_seconds(mean_values)
    events = list(mean_values.columns)
    workstations = list(mean_values.index)
    incoming_prob = incoming_event_odds(events)

    stats = []
    for run in range(args.runs):
        print(f"\nStarting simulation run {run + 1} of {args.runs}")
        simulation = Simulation(mean_values, incoming_prob, events, workstations)
        if args.warmup:
            simulation.simulate(args.max_time / 2)
            simulation.restart()
        simulation.simulate(args.max_time)
        stats.append(simulation.stats)

    process_stats(stats, events.keys())


if __name__ == "__main__":
    main()
